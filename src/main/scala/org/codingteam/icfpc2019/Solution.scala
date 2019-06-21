package org.codingteam.icfpc2019

import scala.collection.immutable.List

sealed abstract class Action
case object MoveUp extends Action {
  override def toString: String = "W"
}

case object MoveDown extends Action {
  override def toString: String = "S"
}

case object MoveLeft extends Action {
  override def toString: String = "A"
}

case object MoveRight extends Action {
  override def toString: String = "D"
}

case object NoOp extends Action {
  override def toString: String = "Z"
}

case object TurnClockwise extends Action {
  override def toString: String = "E"
}

case object TurnCounterClockwise extends Action {
  override def toString: String = "Q"
}

case class AttachManipulator(pos: Pos) extends Action {
  override def toString: String = "B" + pos.toString
}

case object AttachFastWheels extends Action {
  override def toString: String = "F"
}
case object StartDrill extends Action {
  override def toString: String = "L"
}

class Solution(actions : List[Action]) {
  override def toString: String = actions.map(_.toString).mkString("")
}
