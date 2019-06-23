package org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.{Board, Direction}

sealed abstract class Action {
  def apply(board : Board) : Board
}
case object MoveUp extends Action {
  override def toString: String = "W"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x, bot.position.y+1))
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveUp)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object MoveDown extends Action {
  override def toString: String = "S"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x, bot.position.y-1))
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveDown)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object MoveLeft extends Action {
  override def toString: String = "A"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x-1, bot.position.y))
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveLeft)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object MoveRight extends Action {
  override def toString: String = "D"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x+1, bot.position.y))
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveRight)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object NoOp extends Action {
  override def toString: String = "Z"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(NoOp)
    board.tick.copy(solution = newSolution)
  }
}

case object TurnClockwise extends Action {
  override def toString: String = "E"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.clockwise(bot.direction))
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(TurnClockwise)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object TurnCounterClockwise extends Action {
  override def toString: String = "Q"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.counterclockwise(bot.direction))
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(TurnCounterClockwise)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case class AttachManipulator(pos: Pos) extends Action {
  override def toString: String = "B" + pos.toString

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(extraManipulators = bot.extraManipulators + bot.makeRelative(pos))
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(AttachManipulator(pos))
    board.tick.copy(bot = newBot, solution =  newSolution, wrappedCells =  newWrappedCells)
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
    board.tick.copy(remainingDrill = 30, solution = newSolution)
  }
}

class Solution(val reversedActions : Vector[Action]) {
  override def toString: String = reversedActions.reverseIterator.map(_.toString).mkString("")

  override def equals(o: Any): Boolean = {
    o match {
      case that : Solution =>
        this.reversedActions.equals(that.reversedActions)
      case _ => false
    }
  }

  def addAction(action: Action): Solution = new Solution(action +: reversedActions)

  def length(): Double = {
    def actionCost(action: Action): Double = {
      action match {
        case MoveUp | MoveDown | MoveLeft | MoveRight =>
          1.0

        case TurnClockwise | TurnCounterClockwise =>
          0.5

        case _ =>
          1.0
      }
    }

    reversedActions.map(actionCost).sum
  }

  def totalTime: Int = reversedActions.size
}
