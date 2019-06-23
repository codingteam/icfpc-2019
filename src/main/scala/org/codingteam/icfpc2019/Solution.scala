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
    val wrappedCells = bot.wrappedCells(board)
    val newWrappedCells = board.wrappedCells ++
      (wrappedCells ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveUp, newWrappedCells.size - board.wrappedCells.size)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object MoveDown extends Action {
  override def toString: String = "S"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x, bot.position.y-1))
    val wrappedCells = bot.wrappedCells(board)
    val newWrappedCells = board.wrappedCells ++
      (wrappedCells ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveDown, newWrappedCells.size - board.wrappedCells.size)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object MoveLeft extends Action {
  override def toString: String = "A"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x-1, bot.position.y))
    val wrappedCells = bot.wrappedCells(board)
    val newWrappedCells = board.wrappedCells ++
      (wrappedCells ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveLeft, newWrappedCells.size - board.wrappedCells.size)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object MoveRight extends Action {
  override def toString: String = "D"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(position = Pos(bot.position.x+1, bot.position.y))
    val wrappedCells = bot.wrappedCells(board)
    val newWrappedCells = board.wrappedCells ++
      (wrappedCells ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(MoveRight, newWrappedCells.size - board.wrappedCells.size)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object NoOp extends Action {
  override def toString: String = "Z"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(NoOp, 0)
    board.tick.copy(solution = newSolution)
  }
}

case object TurnClockwise extends Action {
  override def toString: String = "E"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.clockwise(bot.direction))
    val wrappedCells = bot.wrappedCells(board)
    val newWrappedCells = board.wrappedCells ++
      (wrappedCells ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(TurnClockwise, newWrappedCells.size - board.wrappedCells.size)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case object TurnCounterClockwise extends Action {
  override def toString: String = "Q"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.counterclockwise(bot.direction))
    val wrappedCells = bot.wrappedCells(board)
    val newWrappedCells = board.wrappedCells ++
      (wrappedCells ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(TurnCounterClockwise, newWrappedCells.size - board.wrappedCells.size)
    board.tick.copy(bot = newBot, solution = newSolution, wrappedCells =  newWrappedCells)
  }
}

case class AttachManipulator(pos: Pos) extends Action {
  override def toString: String = "B" + pos.toString

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(extraManipulators = bot.extraManipulators + bot.makeRelative(pos))
    val wrappedCells = bot.wrappedCells(board)
    val newWrappedCells = board.wrappedCells ++
      (wrappedCells ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    val newSolution = board.solution.addAction(AttachManipulator(pos), newWrappedCells.size - board.wrappedCells.size)
    board.tick.copy(bot = newBot, solution =  newSolution, wrappedCells =  newWrappedCells)
  }
}

case object AttachFastWheels extends Action {
  override def toString: String = "F"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(AttachFastWheels, 0)
    board.tick.copy(remainingFastWheels = 50, solution = newSolution)
  }

}
case object StartDrill extends Action {
  override def toString: String = "L"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(StartDrill, 0)
    board.tick.copy(remainingDrill = 30, solution = newSolution)
  }
}

class Solution(reversedActions : Vector[Action], val effects: Vector[Int]) {
  private val HISTORY_LENGTH = 10

  override def toString: String =
    effects.mkString(",") + "\n" + reversedActions.reverseIterator.map(_.toString).mkString("")

  def addAction(action: Action, effect: Int): Solution =
    new Solution(action +: reversedActions, effects.takeRight(HISTORY_LENGTH - 1) :+ effect)

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
