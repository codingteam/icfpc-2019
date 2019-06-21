package org.codingteam.icfpc2019

import fastparse._, NoWhitespace._

object AppEntry extends App {
  def digit[_: P] = P( CharIn("0-9") )
  def nonzeroDigit[_: P]: P[Unit] = P( CharIn("1-9") )
  def decimalInteger[_: P]: P[BigInt] = P( nonzeroDigit ~ digit.rep | "0" ).!.map(scala.BigInt(_))
  def parsePos[_: P]: P[Pos] = P("(" ~ decimalInteger ~ "," ~ decimalInteger ~ ")").map((coords) => Pos(coords._1, coords._2))

  def parseTaskMap[_: P]: P[TaskMap] = P( parsePos.rep(sep=",") ).map(x => TaskMap(x.toList))

  def parseObstacle[_: P]: P[Obstacle] = P( parsePos.rep(sep=",") ).map(_.toList).map(Obstacle(_))
  def parseObstacles[_: P]: P[List[Obstacle]] = P( parseObstacle.rep(sep=";") ).map(_.toList)

  def parseManipulatorExtension[_: P]: P[ManipulatorExtension] = P( "B" ~ parsePos ).map(ManipulatorExtension(_))
  def parseFastWheels[_: P]: P[FastWheels] = P( "F" ~ parsePos ).map(FastWheels(_))
  def parseDrill[_: P]: P[Drill] = P( "L" ~ parsePos ).map(Drill(_))
  def parseMysteriousPoint[_: P]: P[MysteriousPoint] = P( "X" ~ parsePos ).map(MysteriousPoint(_))
  def parseBooster[_: P]: P[Booster] = P( parseManipulatorExtension | parseFastWheels | parseDrill | parseMysteriousPoint )
  def parseBoosters[_: P]: P[List[Booster]] = P( parseBooster.rep(sep=";") ).map(_.toList)

  def parseTask[_: P]: P[Task] = P( parseTaskMap ~ "#" ~ parsePos ~ "#" ~ parseObstacles ~ "#" ~ parseBoosters ~ End).map((data) => Task(data._1, data._2, data._3, data._4))

  private def run(): Unit = {
    args match {
      case Array("--problem-file", filepath) =>
        println(filepath)

      case _ =>
        println("Run with --problem-file=<filepath.desc> to solve a particular problem")

        val Parsed.Success(value, successIndex) = parse("(0,0),(10,0),(10,10),(0,10)#(0,0)#(4,2),(6,2),(6,7),(4,7);(5,8),(6,8),(6,9),(5,9)#B(0,1);B(1,1);F(0,2);F(1,2);L(0,3);X(0,9)", parseTask(_))
        println(value)
        println(successIndex)
    }
  }

  run()
}
