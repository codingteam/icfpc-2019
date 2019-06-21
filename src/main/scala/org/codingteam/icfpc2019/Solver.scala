package org.codingteam.icfpc2019

import scala.collection.mutable.PriorityQueue

import main.scala.org.codingteam.icfpc2019.{Board, Bot, Direction}

object Solver {
    def solve(task: Task): Solution = {
      val initialBoard = Board(task, Bot(task.startPos, Direction.RIGHT, Set[Pos]()), Set[Pos](), task.obstacles, 0, 0, new Solution(List[Action]()))

      def solutionLength(board: Board): Int = board.solution.length - board.wrappedCells.size

      val open = PriorityQueue[Board](initialBoard)(Ordering.by(solutionLength))
      println("Starting with" + initialBoard.toString)
      var closed = Set[Board]()

      while (open.nonEmpty) {
        println("open   contains " + open.size.toString + " boards")
        println("closed contains " + closed.size.toString + " boards")

        val bestBoard = open.dequeue()
        println("best board is" + bestBoard.toString)
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
          NoOp.apply(bestBoard),
          TurnClockwise.apply(bestBoard),
          TurnCounterClockwise.apply(bestBoard),
          // TODO[M]: Generate all the positions where a manipulator can be attached, and use them to create new Boards
//          AttachManipulator.apply(bestBoard),
          AttachFastWheels.apply(bestBoard),
          StartDrill.apply(bestBoard))

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
