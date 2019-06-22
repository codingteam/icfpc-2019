package org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.{Board, Bot, Direction}

import scala.collection.immutable.List

sealed abstract class Action {
  def apply(board : Board) : Board

  def moveBot(board: Board, fn : Pos => Pos) : Board = {
    val bot = board.bot
    val newPos = fn(bot.position)
    val newBot = bot.copy(position = newPos)
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    board.copy(bot = newBot, wrappedCells = newWrappedCells)
  }
}
case object MoveUp extends Action {
  override def toString: String = "W"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x, pos.y+1)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    val newBoard2 =
      if (ticked.isFastWheelsEnabled())
        moveBot(newBoard, move)
      else
        newBoard
    val newSolution = board.solution.addAction(MoveUp)
    newBoard2.copy(solution = newSolution)
  }
}

case object MoveDown extends Action {
  override def toString: String = "S"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x, pos.y-1)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    val newBoard2 =
      if (ticked.isFastWheelsEnabled())
        moveBot(newBoard, move)
      else
        newBoard
    val newSolution = board.solution.addAction(MoveDown)
    newBoard2.copy(solution = newSolution)
  }
}

case object MoveLeft extends Action {
  override def toString: String = "A"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x-1, pos.y)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    val newBoard2 =
      if (ticked.isFastWheelsEnabled())
        moveBot(newBoard, move)
      else
        newBoard
    val newSolution = board.solution.addAction(MoveLeft)
    newBoard2.copy(solution = newSolution)
  }
}

case object MoveRight extends Action {
  override def toString: String = "D"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x+1, pos.y)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    val newBoard2 =
      if (ticked.isFastWheelsEnabled())
        moveBot(newBoard, move)
      else
        newBoard
    val newSolution = board.solution.addAction(MoveRight)
    newBoard2.copy(solution = newSolution)
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
    val newFastWheels = if (board.isFastWheelsEnabled())
                          board.remainingFastWheels + 50
                        else 50
    println("Attach!")
    board.tick.copy(fastWheelsEnabled = true, remainingFastWheels = newFastWheels, solution = newSolution)
  }

}
case object StartDrill extends Action {
  override def toString: String = "L"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(StartDrill)
    board.tick.copy(remainingDrill = 30, solution = newSolution)
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

        case AttachFastWheels =>
          return 0

        case _ =>
          return 1.0
      }
    }

    reversedActions.map(actionCost(_)).sum
  }
}
