package org.codingteam.icfpc2019

import java.io.{FileOutputStream, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Executors, TimeUnit}
import java.util.zip.{ZipEntry, ZipOutputStream}

import fastparse.NoWhitespace._
import fastparse._
import main.scala.org.codingteam.icfpc2019.{Board, Direction}

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.util.Random

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

  def parseTeleport[_: P]: P[Teleport] = P("R" ~ parsePos).map(Teleport(_))

  def parseClone[_: P]: P[Clone] = P("C" ~ parsePos).map(Clone(_))

  def parseBooster[_: P]: P[Booster] =
    P(parseManipulatorExtension | parseFastWheels | parseDrill | parseMysteriousPoint | parseTeleport | parseClone)

  def parseBoosters[_: P]: P[List[Booster]] = P(parseBooster.rep(sep = ";")).map(_.toList)

  def parseTask[_: P]: P[Task] = P(parseTaskMap ~ "#" ~ parsePos ~ "#" ~ parseObstacles ~ "#" ~ parseBoosters ~ "\n".? ~ End).map((data) => Task(data._1, data._2, data._3, data._4))

  private def run(): Unit = {
    args match {
      case Array("--problem-file", filepath) =>
        solveTask(Paths.get(filepath))

      case Array("--problem-file", "--quiet", filepath) =>
        solveTask(Paths.get(filepath), detailedLogs = false)

      case Array("--directory", directory) => solveDirectory(directory)
      case Array("--directory", directory, "--minutes", minutes) => solveDirectory(directory, minutes)
      case Array("--directory", directory, "--cores", cores) => solveDirectory(directory, _, cores)
      case Array("--directory", directory, "--minutes", minutes, "--cores", cores) => solveDirectory(directory, minutes, cores)

      case Array("--zip", directory) => zipResults(Paths.get(directory))

      case Array("--debug", path, steps) => debug(Paths.get(path), steps.toInt)
      case Array("--debug", path) => debug(Paths.get(path), 0)

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
        println(Direction.diff(Direction.DOWN, Direction.RIGHT))
        println(Direction.diff(Direction.UP, Direction.RIGHT))
        val source = Source.fromFile(filepath)
        val contents = try source.mkString finally source.close()
        val Parsed.Success(task, successIndex) = parse(contents, parseTask(_))
        // TODO[F]: Put the build results into the output
        val board = Board(task)
        println(board)
        println(board.bot.wrappedCells(board))
        //println(Solver.solutionLength(board))
        val newBoard = MoveUp(MoveUp(board))
        println(newBoard)
        println(newBoard.isValid())
        println(newBoard.bot.wrappedCells(newBoard))
        val score = Solver.solutionLength(newBoard)
        println(score)
        println(newBoard.getArea())
        println(newBoard.calcDistanceToUnwrapped(false))
        println(newBoard.bot.wrappedCells(newBoard))

        val ext = Pos(5,3)
        val turned = TurnClockwise(board)
        val board2 = AttachManipulator(turned.bot.makeRelative(ext))(turned)
        println(board2)

        val board3 = TurnCounterClockwise(board2)
        println(board3)
        println(board3.bot.occupiedCells())

      case Array("--test-booster", filepath) =>
        val source = Source.fromFile(filepath)
        val contents = try source.mkString finally source.close()
        val Parsed.Success(task, successIndex) = parse(contents, parseTask(_))
        // TODO[F]: Put the build results into the output
        val board = Board(task)
        val board1 = board.copy(bot = board.bot.copy(position = Pos(9,1)))
        println(board1.currentBooster())
        val board2 = AttachFastWheels(board1)
        println(board2)
        println(Solver.solutionLength(board2))

      case _ =>
        println(
          """Usage:
            |  --problem-file <filepath.desc>
            |    to solve a particular problem
            |  --directory <path> [--minutes <num>] [--cores <num>]
            |    to solve all problems in a directory (recursive)
            |  --zip <path>
            |    to zip all the *.sol files in the output directory
            |  --debug <path-to-desc-or-sol> [steps]
            |    enable interactive interpreter; will skip first [steps]
            |""".stripMargin.format())

        val Parsed.Success(task, successIndex) = parse("(0,0),(10,0),(10,10),(0,10)#(0,0)##", parseTask(_))
//        val Parsed.Success(task, successIndex) = parse("(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(0,3)#(0,0)##", parseTask(_))

        println(task)
        println(successIndex)

        println(Solver.solve(task, Paths.get("con"), true, None))
    }
  }

  private def replaceExtension(path: Path, extension: String): Path = {
    val pathString = path.toString
    val point = pathString.lastIndexOf('.')
    if (point >= 0) Paths.get(pathString.take(point) + "." + extension)
    else Paths.get(pathString + "." + extension)
  }

  run()

  private def parseTaskFile(path: Path) = {
    val source = Source.fromFile(path.toFile)
    val contents = try source.mkString finally source.close()
    parse(contents, parseTask(_))
  }

  private def solveTask(taskFilePath: Path, detailedLogs: Boolean = true, maxDuration: Option[Duration] = None): Boolean = {
    parseTaskFile(taskFilePath) match {
      case error@Parsed.Failure(_, _, _) =>
        println(s"Cannot parse task file $taskFilePath: $error")
        false
      case Parsed.Success(task, _) =>
        val board = Board(task)
        if (detailedLogs) {
          println(task)
          println(board)
        }

        val someSolution = Solver.solve(task, taskFilePath, detailedLogs, maxDuration)
        if (detailedLogs) {
          println(someSolution)
        }
        if (someSolution.isEmpty) {
          println(s"Task could not be solved: ${taskFilePath.getFileName}")
          return false
        }

        val Some(solution) = someSolution

        val outputPath = replaceExtension(taskFilePath, "sol")
        val outputFile = outputPath.toFile

        // TODO[F]: both length() and totalTime are approximations not taking cloning into account
        if (outputFile.exists() && outputFile.length() <= solution.totalTime) {
          println(s"Result is ${if (outputFile.length() == solution.totalTime) "equal to" else "WORSE than"}" +
            s" $outputPath; NOT saving")
          false
        } else {
          val writer = new PrintWriter(outputFile)
          try writer.print(solution) finally writer.close()
          println(s"Result saved to $outputPath")
          true
        }
    }
  }

  private def solveDirectory(pathString: String, minutesString: String = null, coresString: String = null): Unit = {
    val path = Paths.get(pathString)
    val minutes = if (minutesString == null) DEFAULT_MINUTES else minutesString.toInt
    val duration = Duration(minutes, TimeUnit.MINUTES)
    val cores = if (coresString == null) DEFAULT_CORES else coresString.toInt
    val executor = Executors.newFixedThreadPool(cores)
    val solveResults = try {
      implicit val executionContext = ExecutionContext.fromExecutor(executor)

      val files = Files.walk(path).iterator().asScala.filter(_.toString.endsWith(".desc"))
      val futures = Random.shuffle(files).map { file =>
        Future(solveTask(file, false, Some(duration)))
      }.toVector

      println(s"Awaiting for ${futures.size} tasks")
      Await.result(Future.sequence(futures), Duration.Inf)
    } finally {
      executor.shutdown()
    }

    if (solveResults.contains(true)) {
      zipResults(path)
    }
  }

  def zipResults(path: Path): Unit = {
    println("Packing the results...")
    val zipFilePath = path.resolve(s"${path.getFileName}.zip")
    val zipFile = new FileOutputStream(zipFilePath.toFile)
    try {
      val zipArchive = new ZipOutputStream(zipFile)
      try {
        Files.walk(path).iterator().asScala.filter(_.toString.endsWith(".sol")).foreach { file =>
          val entry = new ZipEntry(file.getFileName.toString)
          zipArchive.putNextEntry(entry)
          zipArchive.write(Files.readAllBytes(file))
          zipArchive.closeEntry()
        }
      } finally {
        zipFile.close()
      }
    } finally {
      zipFile.close()
    }

    println(s"Ready: $zipFilePath")
  }

  private def debug(path: Path, startSteps: Int): Unit = {
    val taskDefinitionPath = replaceExtension(path,"desc")
    val solutionPath = replaceExtension(path, "sol")
    println(s"Debugging $path to step $startSteps")

    val Parsed.Success(task, _) = parseTaskFile(taskDefinitionPath)
    val solution = Solution.parse(Source.fromFile(solutionPath.toFile))
    val debugger = new Debugger(task, solution)
    if (startSteps > 0) debugger.skipSteps(startSteps)
    debugger.startInteractive()
  }
}
