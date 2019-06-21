package org.codingteam.icfpc2019

sealed abstract class Booster
case class ManipulatorExtension(pos: Pos) extends Booster
case class FastWheels(pos: Pos) extends Booster
case class Drill(pos: Pos) extends Booster
case class MysteriousPoint(pos: Pos) extends Booster
