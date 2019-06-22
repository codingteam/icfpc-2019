package org.codingteam.icfpc2019

import com.thesamet.spatial.KDTree

import scala.collection.mutable.PriorityQueue
import main.scala.org.codingteam.icfpc2019.{Board, Bot, Direction}

object Solver {
    def solutionLength(board: Board): Double = {
      10*board.wrappedCells.size - board.solution.length - 2*board.frontLength - 5*board.distanceToUnwrapped
      //2*board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped
      //board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped
    }

    def solve(task: Task): Solution = {
      val initialBoard = Board(task)


      val open = PriorityQueue[Board](initialBoard)(Ordering.by(solutionLength))
      println("Starting with\n" + initialBoard.toString)
      var closed = Set[Board]()

      while (open.nonEmpty) {
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
            .filter(!open.iterator.contains(_))

        println("generated " + boardsToCheck.size.toString + " more boards to check")

        for (board <- boardsToCheck) {
          open.enqueue(board)
        }
      }

      new Solution(List[Action]())
    }
  }
