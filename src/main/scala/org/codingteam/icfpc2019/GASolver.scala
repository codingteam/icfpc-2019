package org.codingteam.icfpc2019

import java.nio.file.Path

import main.scala.org.codingteam.icfpc2019.Board
import org.codingteam.icfpc2019.Solver.solutionLength

import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration.Duration

object GASolver {
  def solve(task: Task, filePath: Path, detailedLogs: Boolean, maxDuration: Option[Duration]): Option[Solution] = {
    val fileName = filePath.getFileName
    val initialBoard = Board(task)


    if (detailedLogs) {
      println("Starting with\n" + initialBoard.toString)
    } else {
      println(s"Starting on $fileName")
    }

    var iterationCount = 0
    val startedAt = System.nanoTime()

    while (iterationCount < 1000) {
      iterationCount += 1
      if (maxDuration.isDefined && System.nanoTime() - startedAt > maxDuration.get.toNanos) {
        println(s"$fileName: failed due to timeout")
        return None
      }

      if (detailedLogs) {
      }

      if (initialBoard.isWrapped(detailedLogs)) {
        if (detailedLogs) {
          println("...and it's not wrapped yet")
        }
        return Some(initialBoard.solution)
      }
    }

    println(s"$fileName: cannot find any solution")
    None
  }
}
