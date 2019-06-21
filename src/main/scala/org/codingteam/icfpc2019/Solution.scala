package org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.{Board, Direction}

import scala.collection.immutable.List

sealed abstract class Action {
  def apply(board : Board) : Board
}
case object MoveUp extends Action {
  override def toString: String = "W"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x, bot.position.y+1))
    board.tick.copy(bot = newBot)
  }
}

case object MoveDown extends Action {
  override def toString: String = "S"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x, bot.position.y+1))
    board.tick.copy(bot = newBot)
  }
}

case object MoveLeft extends Action {
  override def toString: String = "A"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x-1, bot.position.y))
    board.tick.copy(bot = newBot)
  }
}

case object MoveRight extends Action {
  override def toString: String = "D"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x+1, bot.position.y))
    board.tick.copy(bot = newBot)
  }
}

case object NoOp extends Action {
  override def toString: String = "Z"

  override def apply(board : Board) : Board = board
}

case object TurnClockwise extends Action {
  override def toString: String = "E"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.clockwise(bot.direction))
    board.tick.copy(bot = newBot)
  }
}

case object TurnCounterClockwise extends Action {
  override def toString: String = "Q"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.counterclockwise(bot.direction))
    board.tick.copy(bot = newBot)
  }
}

case class AttachManipulator(pos: Pos) extends Action {
  override def toString: String = "B" + pos.toString

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(extraManipulators = bot.extraManipulators + pos)
    board.tick.copy(bot = newBot)
  }
}

case object AttachFastWheels extends Action {
  override def toString: String = "F"

  override def apply(board : Board) : Board = {
    board.tick.copy(remainingFastWheels = 50)
  }

}
case object StartDrill extends Action {
  override def toString: String = "L"

  override def apply(board : Board) : Board = {
    board.tick.copy(remainingDrill = 30)
  }
}

class Solution(actions : List[Action]) {
  override def toString: String = actions.map(_.toString).mkString("")
}
