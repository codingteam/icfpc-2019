package org.codingteam.icfpc2019

import java.nio.file.Path

import main.scala.org.codingteam.icfpc2019.Board

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration.Duration

class PriorityQueueSet(queue: mutable.PriorityQueue[Board], set: mutable.Set[Set[Pos]]) {
  def isEmpty: Boolean = queue.isEmpty
  def size: Int = queue.size
  def contains(board: Board): Boolean = set.contains(board.wrappedCells)
  def enqueue(board: Board): Unit = {
    if (!set.contains(board.wrappedCells)) {
      queue.enqueue(board)
      set.add(board.wrappedCells)
    }
  }
  def dequeue(): Board = {
    val board = queue.dequeue()
    set.remove(board.wrappedCells)
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
    type Score = Int

    def distance(pos1: Pos, pos2: Pos): Double = {
      scala.math.sqrt(
        scala.math.pow((pos1.x - pos2.x).toDouble, 2.0) +
        scala.math.pow((pos1.y - pos2.y).toDouble, 2.0)
      )
    }

    //def solutionLength(board: Board): (Double, Double, Int) = {
    def solutionLength(board: Board) = {
      val unwrappedCells = (board.getArea() - board.wrappedCells.size).max(1)
      //10*board.wrappedCells.size - board.solution.length - board.distanceToUnwrapped
      //2*board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped
      //board.wrappedCells.size - board.solution.length() - board.frontLength - board.distanceToUnwrapped

//      val score = board.wrappedCells.size + board.solution.usedBoostersCount() * 50
//      (score, -board.distanceToUnwrapped, - board.solution.length)

      // Efficiency
//      val efficiency: Double = board.wrappedCells.size.toDouble / board.solution.totalTime
//
//      val h: Double = efficiency
//
//      val eta = board.distanceToUnwrapped + unwrappedCells.toDouble / board.bot.occupiedCells().size
//      val k = board.frontLength.toDouble / board.obstaclesFrontLength
//      val projectedEfficiency = scala.math.pow(k, 2.0) * unwrappedCells.toDouble / eta
//
//      val g: Double = projectedEfficiency
//      val f: Double = h + g
//      f

      // Volume
//      val h: Double = board.solution.totalTime
//      val g: Double = (board.frontLength * (board.obstaclesFrontLength - board.obstaclesTouchedLength) - board.wrappedCells.size) / board.solution.totalTime.toDouble
//      val f: Double = h + g
//      -f

//        val h: Double = (board.frontLength * (1.0 + board.obstaclesFrontLength - board.obstaclesTouchedLength) - board.wrappedCells.size) / board.solution.totalTime.toDouble
////        val g: Double = unwrappedCells / (1.0 + board.obstaclesFrontLength - board.obstaclesTouchedLength)
//        val g: Double = unwrappedCells.toDouble / board.getArea()
//        val f: Double = h + g
//        -f

      val volume: Double = (board.frontLength * (board.obstaclesTouchedLength - board.obstaclesFrontLength) - unwrappedCells) / board.solution.totalTime.toDouble
      val corners: Double = board.cornersCount
      (board.wrappedCells.size.toDouble / board.solution.totalTime) * unwrappedCells.toDouble * board.obstaclesTouchedLength / corners / board.frontLength

      val w = board.wrappedCells.size.toDouble / board.getArea()

      val Pos(x, y) = board.task.map.size()
      val diagonal = scala.math.sqrt(scala.math.pow(x.toDouble, 2.0) + scala.math.pow(y.toDouble, 2.0))

      (board.wrappedCells.size.toDouble / (corners+1.0), board.distanceToUnwrapped, -board.solution.totalTime)
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
        mutable.Set[Set[Pos]]()
      )
      if (detailedLogs) {
        println("Starting with\n" + initialBoard.toString)
      } else {
        println(s"Starting on $fileName")
      }
      var closed = mutable.Set[Board]()

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
          println("  obstacle touched length = " + bestBoard.obstaclesTouchedLength)
        } else if (iterationCount % 1000 == 0) {
          println(s"$fileName heuristics: ${solutionLength(bestBoard)}")
        }
        if (bestBoard.isWrapped(detailedLogs)) {
          if (detailedLogs) {
            println("...and it's not wrapped yet")
          }
          return Some(bestBoard.solution)
        }

        closed += bestBoard

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

        if (bestBoard.teleportsCount > 0) {
          val withTeleportInstalled = Reset(bestBoard)
          neighbours = withTeleportInstalled +: (trivialNeighbours(withTeleportInstalled) ++ neighbours)
        }

        for (teleport <- bestBoard.installedTeleports) {
          val afterTeleporting = Shift(teleport)(bestBoard)
          neighbours = afterTeleporting +: (trivialNeighbours(afterTeleporting) ++ neighbours)
        }

        var newNeighbours = List[Board]()
        for (neighbour <- neighbours) {
          if (neighbour.isValid() && !neighbour.isWrapped(false) && neighbour.distanceToUnwrapped > 1) {
            var acc = List[Board](neighbour.copy())
            do {
              acc = acc
                .flatMap(trivialNeighbours(_))
                .filter(_.isValid())
                .filter(_.distanceToUnwrapped < acc.head.distanceToUnwrapped)
            } while (acc.nonEmpty && acc.forall(_.distanceToUnwrapped > 1))

            newNeighbours = acc ++ newNeighbours
          }
        }

        neighbours = newNeighbours ++ neighbours

          //TurnCounterClockwise.apply(bestBoard),
          // TODO[M]: Generate all the positions where a manipulator can be attached, and use them to create new Boards
//          AttachManipulator.apply(bestBoard),
//          AttachFastWheels.apply(bestBoard),
//          StartDrill.apply(bestBoard)
       // )

        val boardsToCheck = neighbours
            .filter(_.isValid())
            .filter(!open.contains(_))

        if (detailedLogs) {
          println("generated " + boardsToCheck.size.toString + " more boards to check")
        }

//        for (board <- boardsToCheck) {
//          var found = false
//
//          for (seen <- closed) {
//            if (seen.wrappedCells == board.wrappedCells) {
//              found = true
//
//              if (seen.solution.totalTime > board.solution.totalTime) {
//                closed -= seen
//                open.enqueue(board)
//              } else {
//                // We've seen the same cells covered in a faster way. Ignore this board
//              }
//            }
//          }
//
//          if (!found) {
//            open.enqueue(board)
//          }
//        }

        for (board <- boardsToCheck) {
          val found = closed.find(_.wrappedCells == board.wrappedCells).isDefined
          if (!found) {
            open.enqueue(board)
          }
        }
      }

      println(s"$fileName: cannot find any solution")
      None
    }
  }
