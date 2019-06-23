package org.codingteam.icfpc2019

sealed abstract class Booster {
  val pos : Pos

  def symbol() : String
}

case class ManipulatorExtension(pos : Pos) extends Booster {
  override def symbol() : String = "B"
}
case class FastWheels(pos: Pos) extends Booster {
  override def symbol(): String = "F"
}
case class Drill(pos: Pos) extends Booster {
  override def symbol(): String = "D"
}
case class MysteriousPoint(pos: Pos) extends Booster {
  override def symbol(): String = "X"
}
