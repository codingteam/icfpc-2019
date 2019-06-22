package org.codingteam.icfpc2019

import java.util.concurrent.Executors

import com.thesamet.spatial.KDTree
import main.scala.org.codingteam.icfpc2019.Board

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

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
  private implicit val executor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8))

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

      val euclideanToAllNearest =
        board.bot.occupiedCells()
          .flatMap(pos => kdTree.findNearest((pos.x, pos.y), 1)
            .map((coords) => distance(Pos(coords._1, coords._2), pos)))

      var euclideanToNearest = 0.0
      if (euclideanToAllNearest.nonEmpty) {
        euclideanToNearest = euclideanToAllNearest.min
      }

      1.9*board.wrappedCells.size - euclideanToNearest - board.solution.length
    }

  var tasks = Map[Board, Future[Double]]()
  def solutionLengthThreaded(board: Board): Double = {
    val future = tasks.getOrElse(board, Future(solutionLength(board)))
    Await.result(future, Duration.Inf)
  }

    def solve(task: Task): Solution = {
      val initialBoard = Board(task)


      val open = new PriorityQueueSet(
        PriorityQueue[Board](initialBoard)(Ordering.by(solutionLengthThreaded)),
        mutable.Set[Board]()
      )
      println("Starting with\n" + initialBoard.toString)
      var closed = Set[Board]()

      while (!open.isEmpty) {
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
            .filter(!open.contains(_))

        println("generated " + boardsToCheck.size.toString + " more boards to check")

        for (board <- boardsToCheck) {
          open.enqueue(board)
          tasks.getOrElse(board, Future(solutionLength(board)))
        }
      }

      new Solution(List[Action]())
    }
  }
