package org.codingteam.icfpc2019

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Executors, TimeUnit}

import fastparse.NoWhitespace._
import fastparse._
import main.scala.org.codingteam.icfpc2019.Board

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

object AppEntry extends App {
  private val DEFAULT_MINUTES = 10
  private lazy val DEFAULT_CORES = Runtime.getRuntime.availableProcessors()

  def digit[_: P] = P(CharIn("0-9"))

  def nonzeroDigit[_: P]: P[Unit] = P(CharIn("1-9"))

  def decimalInteger[_: P]: P[BigInt] = P(nonzeroDigit ~ digit.rep | "0").!.map(scala.BigInt(_))

  def parsePos[_: P]: P[Pos] = P("(" ~ decimalInteger ~ "," ~ decimalInteger ~ ")").map((coords) => Pos(coords._1, coords._2))

  def parseTaskMap[_: P]: P[TaskMap] = P(parsePos.rep(sep = ",")).map(x => TaskMap(x.toList))

  def parseManipulatorExtension[_: P]: P[ManipulatorExtension] = P("B" ~ parsePos).map(ManipulatorExtension(_))
  def parseObstacle[_: P]: P[Obstacle] = P( parsePos.rep(min=2, sep=",") ).map(_.toList).map(Obstacle(_))
  def parseObstacles[_: P]: P[List[Obstacle]] = P( parseObstacle.rep(sep=";") ).map(_.toList)

  def parseFastWheels[_: P]: P[FastWheels] = P("F" ~ parsePos).map(FastWheels(_))

  def parseDrill[_: P]: P[Drill] = P("L" ~ parsePos).map(Drill(_))

  def parseMysteriousPoint[_: P]: P[MysteriousPoint] = P("X" ~ parsePos).map(MysteriousPoint(_))

  def parseBooster[_: P]: P[Booster] = P(parseManipulatorExtension | parseFastWheels | parseDrill | parseMysteriousPoint)

  def parseBoosters[_: P]: P[List[Booster]] = P(parseBooster.rep(sep = ";")).map(_.toList)

  def parseTask[_: P]: P[Task] = P(parseTaskMap ~ "#" ~ parsePos ~ "#" ~ parseObstacles ~ "#" ~ parseBoosters ~ "\n".? ~ End).map((data) => Task(data._1, data._2, data._3, data._4))

  private def run(): Unit = {
    args match {
      case Array("--problem-file", filepath) =>
        solveTask(Paths.get(filepath))

      case Array("--directory", directory) => solveDirectory(directory)
      case Array("--directory", directory, "--minutes", minutes) => solveDirectory(directory, minutes)
      case Array("--directory", directory, "--cores", cores) => solveDirectory(directory, _, cores)
      case Array("--directory", directory, "--minutes", minutes, "--cores", cores) => solveDirectory(directory, minutes, cores)

      case Array("--test-awt") =>
        val obstacle = Obstacle(List(
          Pos(4, 6),
          Pos(4, 12),
          Pos(7, 12),
          Pos(7, 8),
          Pos(10, 8),
          Pos(10, 12),
          Pos(10, 6),
        ))
        val results = for (y <- 0 until 15) yield {
          for (x <- 0 until 15) yield {
            val pos = Pos(x, y)
            obstacle containsPosition pos
          }
        }
        val s = results.map(_.map(if (_) "+" else "-").mkString("")).mkString("\n")
        println(s)

      case Array("--test-check", filepath) =>
        val source = Source.fromFile(filepath)
        val contents = try source.mkString finally source.close()
        val Parsed.Success(task, successIndex) = parse(contents, parseTask(_))
        // TODO[F]: Put the build results into the output
        val board = Board(task)
        val newBoard = MoveUp(MoveUp(board))
        println(newBoard)
        println(newBoard.isValid())
        val score = Solver.solutionLength(newBoard)
        println(score)
        println(newBoard.getArea())
        println(newBoard.calcDistanceToUnwrapped(false))

      case _ =>
        println(
          """Usage:
            |  --problem-file <filepath.desc>
            |    to solve a particular problem
            |  --directory <path> [--minutes <num>] [--cores <num>]
            |    to solve all problems in a directory (recursive)
            |""".stripMargin.format())

        val Parsed.Success(task, successIndex) = parse("(0,0),(10,0),(10,10),(0,10)#(0,0)##", parseTask(_))
//        val Parsed.Success(task, successIndex) = parse("(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(0,3)#(0,0)##", parseTask(_))

        println(task)
        println(successIndex)

        println(Solver.solve(task, Paths.get("con"), true, None))
    }
  }

  run()

  private def solveTask(taskFilePath: Path, detailedLogs: Boolean = true, maxDuration: Option[Duration] = None): Unit = {
    val source = Source.fromFile(taskFilePath.toFile)
    val contents = try source.mkString finally source.close()
    val Parsed.Success(task, successIndex) = parse(contents, parseTask(_))

    val board = Board(task)
    if (detailedLogs) {
      println(task)
      println(board)
      // println(successIndex)
    }

    val someSolution = Solver.solve(task, taskFilePath, detailedLogs, maxDuration)
    if (detailedLogs) {
      println(someSolution)
    }
    if (someSolution.isEmpty) {
      println(s"Task could not be solved: ${taskFilePath.getFileName}")
      return
    }

    val Some(solution) = someSolution

    def replaceExtension(fileName: String, extension: String): String = {
      val point = fileName.lastIndexOf('.')
      if (point >= 0) fileName.take(point) + "." + extension
      else fileName + "." + extension
    }

    val outputPath = replaceExtension(taskFilePath.toString, "sol")
    val outputFile = new File(outputPath)

    // TODO[F]: both length() and totalTime are approximations not taking cloning into account
    if (outputFile.exists() && outputFile.length() <= solution.totalTime) {
      println(s"Result is ${if (outputFile.length() == solution.totalTime) "equal" else "WORSE"}" +
        s" than ${outputPath}; NOT saving")
    } else {
      val writer = new PrintWriter(outputFile)
      try writer.print(solution) finally writer.close()
      println(s"Result saved to ${outputPath}")
    }
  }

  private def solveDirectory(pathString: String, minutesString: String = null, coresString: String = null): Unit = {
    import scala.collection.JavaConverters._

    val path = Paths.get(pathString)
    val minutes = if (minutesString == null) DEFAULT_MINUTES else minutesString.toInt
    val duration = Duration(minutes, TimeUnit.MINUTES)
    val cores = if (coresString == null) DEFAULT_CORES else coresString.toInt
    implicit val executor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(cores))

    val futures = Files.walk(path).iterator().asScala.filter(_.toString.endsWith(".desc")).map { file =>
      Future(solveTask(file, false, Some(duration)))
    }.toVector

    println(s"Awaiting for ${futures.size} tasks")
    Await.result(Future.sequence(futures), Duration.Inf)
  }
}
