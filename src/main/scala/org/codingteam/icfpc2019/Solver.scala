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

      val score = board.wrappedCells.size + board.solution.usedBoostersCount() * 50
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

      var board = Board(task)

      if (detailedLogs) {
        println("Starting with\n" + board.toString)
      } else {
        println(s"Starting on $fileName")
      }

      var iterationCount = 0
      val startedAt = System.nanoTime()

      while (!board.isWrapped(detailedLogs)) {
        iterationCount += 1
        if (maxDuration.isDefined && System.nanoTime() - startedAt > maxDuration.get.toNanos) {
          println(s"$fileName: failed due to timeout")
          return None
        }

        val neighbours = trivialNeighbours(board)

        val boardsToCheck = neighbours
            .filter(_.isValid())
            .sortBy(b => (-b.wrappedCells.size, b.distanceToUnwrapped))

        if (boardsToCheck.isEmpty) {
          return None
        } else {
          board = boardsToCheck.head
        }
      }

      return Some(board.solution)
    }
  }
