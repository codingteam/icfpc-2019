package org.codingteam.icfpc2019

import java.nio.file.Path

import main.scala.org.codingteam.icfpc2019.Board

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration.Duration

class PriorityQueueSet(queue: mutable.PriorityQueue[Board], set: mutable.Set[Board]) {
  def isEmpty: Boolean = queue.isEmpty
  def size: Int = queue.size
  def contains(board: Board): Boolean = set.contains(board)
  def enqueue(board: Board): Unit = {
    queue.enqueue(board)
    set.add(board)
  }
  def dequeue(): Board = {
    val board = queue.dequeue()
    set.remove(board)
    board
  }
}

object Solver {

    def distance(pos1: Pos, pos2: Pos): Double = {
      scala.math.sqrt(
        scala.math.pow((pos1.x - pos2.x).toDouble, 2.0) +
        scala.math.pow((pos1.y - pos2.y).toDouble, 2.0)
      )
    }

    def solutionLength(board: Board) = {
      //val unwrappedCells = (board.getArea() - board.wrappedCells.size).max(1)
      //10*board.wrappedCells.size - board.solution.length - board.distanceToUnwrapped
      //2*board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped
      //board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped
      val score = (board.solution.effects.sum / board.solution.effects.length.toDouble) / 4000 + board.wrappedCells.size / board.getArea().toDouble
//      val lastSpeed =
//      if (board.distanceToUnwrapped > 5)
//        score = 0
      (score, -board.distanceToUnwrapped, - board.solution.length)
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
      var closed = Map[Set[Pos], Board]()

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

        closed.get(bestBoard.wrappedCells) match {
          case None => closed = closed.updated(bestBoard.wrappedCells, bestBoard)
          case Some(board2) if board2.solution.length > bestBoard.solution.length =>
            closed = closed.updated(bestBoard.wrappedCells, bestBoard)
          case _ =>
        }

        var neighbours = List[Board](
          MoveUp.apply(bestBoard),
          MoveDown.apply(bestBoard),
          MoveLeft.apply(bestBoard),
          MoveRight.apply(bestBoard)
        )
        //          NoOp.apply(bestBoard),

        val clockwise : Board = TurnClockwise.apply(bestBoard)
        if (clockwise.wrappedCells.size > bestBoard.wrappedCells.size)
          neighbours = clockwise :: neighbours

        val counterclockwise : Board = TurnCounterClockwise.apply(bestBoard)
        if (counterclockwise.wrappedCells.size > bestBoard.wrappedCells.size)
          neighbours = counterclockwise :: neighbours

          //TurnCounterClockwise.apply(bestBoard),
          // TODO[M]: Generate all the positions where a manipulator can be attached, and use them to create new Boards
//          AttachManipulator.apply(bestBoard),
//          AttachFastWheels.apply(bestBoard),
//          StartDrill.apply(bestBoard)
       // )

        val boardsToCheck = neighbours
            .filter(_.isValid())
            .filter { board1 => closed.get(board1.wrappedCells) match {
              case None => true
              case Some(board2) if board2.solution.length > board1.solution.length => true
              case _ => false
            } }
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
