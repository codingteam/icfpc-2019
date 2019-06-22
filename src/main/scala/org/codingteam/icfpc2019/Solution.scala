package org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.{Board, Bot, Direction}

import scala.collection.immutable.List

sealed abstract class Action {
  def apply(board : Board) : Board

  def tickAndReplaceBotWith(board: Board, action: Action, newBot: Bot): Board = {
    val bot = board.bot
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(action)

    var newBoosters = board.boosters
    var newDrills = board.remainingDrills
    val drill = Drill(newBot.position)
    if (newBoosters.contains(drill)) {
      newBoosters = newBoosters - drill
      newDrills += 1
    }

    board.tick.copy(
      bot = newBot,
      solution = newSolution,
      wrappedCells = newWrappedCells,
      boosters = newBoosters,
      remainingDrills = newDrills)
  }

  def tickAndMoveBotBy(board: Board, action: Action, dx: Pos): Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x + dx.x, bot.position.y+ dx.y))
    tickAndReplaceBotWith(board, action, newBot)
  }
}
case object MoveUp extends Action {
  override def toString: String = "W"

  override def apply(board : Board) : Board = {
    tickAndMoveBotBy(board, MoveUp, Pos(0, 1))
  }
}

case object MoveDown extends Action {
  override def toString: String = "S"

  override def apply(board : Board) : Board = {
    tickAndMoveBotBy(board, MoveDown, Pos(0, -1))
  }
}

case object MoveLeft extends Action {
  override def toString: String = "A"

  override def apply(board : Board) : Board = {
    tickAndMoveBotBy(board, MoveLeft, Pos(-1, 0))
  }
}

case object MoveRight extends Action {
  override def toString: String = "D"

  override def apply(board : Board) : Board = {
    tickAndMoveBotBy(board, MoveRight, Pos(1, 0))
  }
}

case object NoOp extends Action {
  override def toString: String = "Z"

  override def apply(board : Board) : Board = {
    tickAndMoveBotBy(board, NoOp, Pos(0, 0))
  }
}

case object TurnClockwise extends Action {
  override def toString: String = "E"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.clockwise(bot.direction))
    tickAndReplaceBotWith(board, TurnClockwise, newBot)
  }
}

case object TurnCounterClockwise extends Action {
  override def toString: String = "Q"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.counterclockwise(bot.direction))
    tickAndReplaceBotWith(board, TurnCounterClockwise, newBot)
  }
}

case class AttachManipulator(pos: Pos) extends Action {
  override def toString: String = "B" + pos.toString

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(extraManipulators = bot.extraManipulators + bot.makeRelative(pos))
    tickAndReplaceBotWith(board, AttachManipulator(pos), newBot)
  }
}

case object AttachFastWheels extends Action {
  override def toString: String = "F"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(AttachFastWheels)
    board.tick.copy(remainingFastWheels = 50, solution = newSolution)
  }

}
case object StartDrill extends Action {
  override def toString: String = "L"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(StartDrill)
    board.tick.copy(remainingDrillTicks = 30, solution = newSolution)
  }
}

class Solution(reversedActions : List[Action]) {
  override def toString: String = reversedActions.reverse.map(_.toString).mkString("")

  def addAction(action: Action): Solution = return new Solution(action +: reversedActions)

  def length(): Double = {
    def actionCost(action: Action): Double = {
      action match {
        case MoveUp | MoveDown | MoveLeft | MoveRight =>
          return 1.0

        case TurnClockwise | TurnCounterClockwise =>
          return 0.5

        case _ =>
          return 1.0
      }
    }

    reversedActions.map(actionCost(_)).sum
  }
}
