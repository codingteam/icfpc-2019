package org.codingteam.icfpc2019

import com.thesamet.spatial.KDTree
import main.scala.org.codingteam.icfpc2019.Board

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

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

    def solutionLength(board: Board): Double = {
      50*board.wrappedCells.size - board.solution.length - 5*board.calcFrontLength() - 5*board.distanceToUnwrapped
      //board.wrappedCells.size - board.solution.length() - board.calcFrontLength() - board.distanceToUnwrapped
    }

    def solve(task: Task): Solution = {
      val initialBoard = Board(task)


      val open = new PriorityQueueSet(
        PriorityQueue[Board](initialBoard)(Ordering.by(solutionLength)),
        mutable.Set[Board]()
      )
      println("Starting with\n" + initialBoard.toString)
      var closed = Set[Board]()

      while (!open.isEmpty) {
        println("open   contains " + open.size.toString + " boards")
        println("closed contains " + closed.size.toString + " boards")

        val bestBoard = open.dequeue()
        println("best board is\n" + bestBoard.toString + " with score of " + solutionLength(bestBoard))
        println("  and distance " + bestBoard.distanceToUnwrapped.toString)
        if (bestBoard.isWrapped()) {
          println("...and it's not wrapped yet")
          return bestBoard.solution
        }

        closed = closed + bestBoard

        val neighbours = List[Board](
          MoveUp.apply(bestBoard),
          MoveDown.apply(bestBoard),
          MoveLeft.apply(bestBoard),
          MoveRight.apply(bestBoard),
//          NoOp.apply(bestBoard),
          TurnClockwise.apply(bestBoard),
          TurnCounterClockwise.apply(bestBoard),
          // TODO[M]: Generate all the positions where a manipulator can be attached, and use them to create new Boards
//          AttachManipulator.apply(bestBoard),
//          AttachFastWheels.apply(bestBoard),
//          StartDrill.apply(bestBoard)
        )

        val boardsToCheck = neighbours
            .filter(_.isValid())
            .filter(!closed.contains(_))
            .filter(!open.contains(_))

        println("generated " + boardsToCheck.size.toString + " more boards to check")

        for (board <- boardsToCheck) {
          open.enqueue(board)
        }
      }

      new Solution(List[Action]())
    }
  }
