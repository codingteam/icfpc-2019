package org.codingteam.icfpc2019

import java.nio.file.Path

import main.scala.org.codingteam.icfpc2019.Board

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration.Duration

class PriorityQueueSet(queue: mutable.PriorityQueue[Board], set: mutable.Set[Board]) {
  def isEmpty: Boolean = queue.isEmpty
  def size: Int = queue.size
  def contains(board: Board): Boolean = set.contains(board.withoutSolution())
  def enqueue(board: Board): Unit = {
    queue.enqueue(board)
    set.add(board.withoutSolution())
  }
  def dequeue(): Board = {
    val board = queue.dequeue()
    set.remove(board.withoutSolution())
    board
  }
}

//case class Score(score : Double, distance : Int, length : Double) extends Ordered[Score] {
//  override def compare(that: Score): Int = {
//    if ((this.score - that.score).abs <= 1) {
//      this.scalar().compare(that.scalar())
//    } else {
//      this.score.compare(that.score)
//    }
//  }
//
//  def scalar() : Double = {
//    10 * score + 5 * distance + length
//  }
//}

object Solver {

    def distance(pos1: Pos, pos2: Pos): Double = {
      scala.math.sqrt(
        scala.math.pow((pos1.x - pos2.x).toDouble, 2.0) +
        scala.math.pow((pos1.y - pos2.y).toDouble, 2.0)
      )
    }

    //def solutionLength(board: Board): (Double, Double, Int) = {
    def solutionLength(board: Board): (Double, Int, Double) = {
      //val unwrappedCells = (board.getArea() - board.wrappedCells.size).max(1)
      //10*board.wrappedCells.size - board.solution.length - board.distanceToUnwrapped
      //2*board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped
      //board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped

      val score = board.wrappedCells.size
      (score, -board.distanceToUnwrapped, - board.solution.length)
    }

    def trivialNeighbours(board: Board): List[Board] = {
      var neighbours = List[Board](
        MoveUp.apply(board),
        MoveDown.apply(board),
        MoveLeft.apply(board),
        MoveRight.apply(board)
      )

      val clockwise : Board = TurnClockwise.apply(board)
      if (clockwise.wrappedCells.size > board.wrappedCells.size)
        neighbours = clockwise :: neighbours

      val counterclockwise : Board = TurnCounterClockwise.apply(board)
      if (counterclockwise.wrappedCells.size > board.wrappedCells.size)
        neighbours = counterclockwise :: neighbours

      neighbours
    }

    def solve(task: Task, filePath: Path, detailedLogs: Boolean, maxDuration: Option[Duration]): Option[Solution] = {
      val fileName = filePath.getFileName
      val initialBoard = Board(task)


      val open = new PriorityQueueSet(
        PriorityQueue[Board](initialBoard)(Ordering.by(solutionLength)),
        mutable.Set[Board]()
      )
      if (detailedLogs) {
        println("Starting with\n" + initialBoard.toString)
      } else {
        println(s"Starting on $fileName")
      }
      var closed = Set[Board]()

      var iterationCount = 0
      val startedAt = System.nanoTime()

      while (!open.isEmpty) {
        iterationCount += 1
        if (maxDuration.isDefined && System.nanoTime() - startedAt > maxDuration.get.toNanos) {
          println(s"$fileName: failed due to timeout")
          return None
        }

        if (detailedLogs) {
          println("open   contains " + open.size.toString + " boards")
          println("closed contains " + closed.size.toString + " boards")
        }

        val bestBoard = open.dequeue()
        if (detailedLogs) {
          println("best board is\n" + bestBoard.toString + " with score of " + solutionLength(bestBoard))
          println("  and distance " + bestBoard.distanceToUnwrapped.toString)
        } else if (iterationCount % 1000 == 0) {
          println(s"$fileName heuristics: ${solutionLength(bestBoard)}")
        }
        if (bestBoard.isWrapped(detailedLogs)) {
          if (detailedLogs) {
            println("...and it's not wrapped yet")
          }
          return Some(bestBoard.solution)
        }

        closed = closed + bestBoard.withoutSolution()

        var neighbours = trivialNeighbours(bestBoard)

        neighbours = trivialNeighbours(TurnClockwise(bestBoard)) ++ neighbours

        neighbours = trivialNeighbours(TurnCounterClockwise(bestBoard)) ++ neighbours

        if (bestBoard.remainingDrills > 0) {
          val withDrill = StartDrill(bestBoard)
          neighbours = withDrill +: (trivialNeighbours(withDrill) ++ neighbours)
        }

        if (bestBoard.hasFastWheels && ! bestBoard.fastWheelsEnabled) {
          val withWheels = AttachFastWheels(bestBoard)
          neighbours = withWheels +: (trivialNeighbours(withWheels) ++ neighbours)
        }

        if (bestBoard.teleportsCount > 0)
          neighbours = Reset(bestBoard) :: neighbours

        for (teleport <- bestBoard.installedTeleports) {
          neighbours = Shift(teleport)(bestBoard) :: neighbours
        }

          //TurnCounterClockwise.apply(bestBoard),
          // TODO[M]: Generate all the positions where a manipulator can be attached, and use them to create new Boards
//          AttachManipulator.apply(bestBoard),
//          AttachFastWheels.apply(bestBoard),
//          StartDrill.apply(bestBoard)
       // )

        val boardsToCheck = neighbours
            .filter(_.isValid())
            .filter(b => !closed.contains(b.withoutSolution()))
            .filter(!open.contains(_))

        if (detailedLogs) {
          println("generated " + boardsToCheck.size.toString + " more boards to check")
        }

        for (board <- boardsToCheck) {
          open.enqueue(board)
        }
      }

      println(s"$fileName: cannot find any solution")
      None
    }
  }
