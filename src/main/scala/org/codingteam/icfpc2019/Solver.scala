package org.codingteam.icfpc2019

import com.thesamet.spatial.KDTree

import scala.collection.mutable.PriorityQueue
import main.scala.org.codingteam.icfpc2019.{Board, Bot, Direction}

object Solver {
    def distance(pos1: Pos, pos2: Pos): Double = {
      scala.math.sqrt(
        scala.math.pow((pos1.x - pos2.x).toDouble, 2.0) +
        scala.math.pow((pos1.y - pos2.y).toDouble, 2.0)
      )
    }

    def solutionLength(board: Board): Double = {
      val allCellsCoords = for {x <- BigInt(0) until board.task.map.maxX; y <- BigInt(0) until board.task.map.maxY if board.isValidPosition(Pos(x,y))}
          yield (x, y)
      val unwrappedCells = allCellsCoords.filter((coords) => !board.wrappedCells.contains(Pos(coords._1, coords._2)))

      val kdTree = KDTree.fromSeq(unwrappedCells)

      import scala.math.Ordering.Implicits._

      val euclideanToAllNearest =
        board.bot.occupiedCells()
          .flatMap(pos => kdTree.findNearest((pos.x, pos.y), 1)
            .map((coords) => distance(Pos(coords._1, coords._2), pos)))

      var euclideanToNearest = 0.0
      if (euclideanToAllNearest.nonEmpty) {
        euclideanToNearest = euclideanToAllNearest.min
      }

      5*board.wrappedCells.size - euclideanToNearest - board.solution.length
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
