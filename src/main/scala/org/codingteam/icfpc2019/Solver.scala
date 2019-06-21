package org.codingteam.icfpc2019

import scala.collection.mutable.PriorityQueue

import main.scala.org.codingteam.icfpc2019.{Board, Bot, Direction}

object Solver {
    def solve(task: Task): Solution = {
      val initialBoard = Board(task)

      def solutionLength(board: Board): Int = {
        val x = board.bot.position.x
        val y = board.bot.position.y

        val neighbours = List[Pos](
          Pos(x-1, y-1), Pos(x-1, y), Pos(x-1, y+1),
          Pos(x, y-1), Pos(x, y), Pos(x, y+1),
          Pos(x+1, y-1), Pos(x+1, y), Pos(x+1, y+1))
        val couldWrap = neighbours
          .filter(pos => pos.isValid())
          .filter(board.isValidPosition(_))
          .filter(!board.wrappedCells.contains(_))
        board.wrappedCells.size - board.solution.length - couldWrap.size
      }

      val open = PriorityQueue[Board](initialBoard)(Ordering.by(solutionLength))
      println("Starting with" + initialBoard.toString)
      var closed = Set[Board]()

      while (open.nonEmpty) {
        println("open   contains " + open.size.toString + " boards")
        println("closed contains " + closed.size.toString + " boards")

        val bestBoard = open.dequeue()
        println("best board is\n" + bestBoard.toString)
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
