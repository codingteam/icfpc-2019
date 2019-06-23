package org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.{Board, Bot, Direction}

sealed abstract class Action {
  def apply(board : Board) : Board

  def moveBot(board: Board, fn : Pos => Pos) : Board = {
    val bot = board.bot
    val newPos = fn(bot.position)
    val newBot = bot.copy(position = newPos)
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)

    var drilled = board.drilledObstacles
    if (board.remainingDrillTicks > 0 && board.obstacles.exists(o => o.containsPosition(newBot.position)))
      drilled = drilled + newBot.position

    board.copy(bot = newBot, wrappedCells = newWrappedCells, drilledObstacles = drilled)
  }
}
case object MoveUp extends Action {
  override def toString: String = "W"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x, pos.y+1)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    if (! newBoard.isValid()) {
      newBoard
    } else {
      val newBoard2 =
        if (ticked.isFastWheelsEnabled()) {
          val b = moveBot(newBoard, move)
          if (b.isValid()) b else newBoard
        } else
          newBoard
      val newSolution = board.solution.addAction(MoveUp)
      newBoard2.copy(solution = newSolution)
    }
  }
}

case object MoveDown extends Action {
  override def toString: String = "S"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x, pos.y-1)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    if (! newBoard.isValid()) {
      newBoard
    } else {
      val newBoard2 =
        if (ticked.isFastWheelsEnabled()) {
          val b = moveBot(newBoard, move)
          if (b.isValid()) b else newBoard
        } else
          newBoard
      val newSolution = board.solution.addAction(MoveDown)
      newBoard2.copy(solution = newSolution)
    }
  }
}

case object MoveLeft extends Action {
  override def toString: String = "A"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x-1, pos.y)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    if (! newBoard.isValid()) {
      newBoard
    } else {
      val newBoard2 =
        if (ticked.isFastWheelsEnabled()) {
          val b = moveBot(newBoard, move)
          if (b.isValid()) b else newBoard
        } else
          newBoard
      val newSolution = board.solution.addAction(MoveLeft)
      newBoard2.copy(solution = newSolution)
    }
  }
}

case object MoveRight extends Action {
  override def toString: String = "D"

  override def apply(board : Board) : Board = {
    val move : Pos => Pos = pos => Pos(pos.x+1, pos.y)
    val ticked = board.tick
    val newBoard = moveBot(ticked, move)
    if (! newBoard.isValid()) {
      newBoard
    } else {
      val newBoard2 =
        if (ticked.isFastWheelsEnabled()) {
          val b = moveBot(newBoard, move)
          if (b.isValid()) b else newBoard
        } else
          newBoard
      val newSolution = board.solution.addAction(MoveRight)
      newBoard2.copy(solution = newSolution)
    }
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
    val newSolution = board.solution.addAction(TurnClockwise)
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    board.tick.copy(solution = newSolution, bot = newBot, wrappedCells = newWrappedCells)
  }
}

case object TurnCounterClockwise extends Action {
  override def toString: String = "Q"

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(direction = Direction.counterclockwise(bot.direction))
    val newSolution = board.solution.addAction(TurnCounterClockwise)
    val newWrappedCells = board.wrappedCells ++
      (bot.wrappedCells(board) ++ newBot.wrappedCells(board))
        .filter(pos => pos.x < board.task.map.maxX && pos.y < board.task.map.maxY)
    board.tick.copy(solution = newSolution, bot = newBot, wrappedCells = newWrappedCells)
  }
}

case class AttachManipulator(pos: Pos) extends Action {
  override def toString: String = "B" + pos.toString

  override def apply(board : Board) : Board = {
    val bot = board.bot
    val newBot = bot.copy(extraManipulators = bot.extraManipulators + bot.makeRelative(pos))
    val newSolution = board.solution.addAction(AttachManipulator(pos))
    board.tick.copy(solution = newSolution, bot = newBot)
  }
}

case object AttachFastWheels extends Action {
  override def toString: String = "F"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(AttachFastWheels)
    // We always tick the board *before* performing a movement, so we have to +1 the counter to compensate for that
    val newFastWheels = 50 + 1
    println("Attach!")
    board.tick.copy(fastWheelsEnabled = true, remainingFastWheels = newFastWheels, solution = newSolution)
  }

}
case object StartDrill extends Action {
  override def toString: String = "L"

  override def apply(board : Board) : Board = {
    val newSolution = board.solution.addAction(StartDrill)
    val newRemainingDrills = board.remainingDrills - 1
    // We always tick the board *before* performing a movement, so we have to +1 the counter to compensate for that
    board.tick.copy(remainingDrillTicks = 30 + 1, solution = newSolution, remainingDrills = newRemainingDrills)
  }
}

class Solution(reversedActions : Vector[Action]) {
  override def toString: String = reversedActions.reverseIterator.map(_.toString).mkString("")

  def addAction(action: Action): Solution = new Solution(action +: reversedActions)

  def length(): Double = {
    def actionCost(action: Action): Double = {
      action match {
        case MoveUp | MoveDown | MoveLeft | MoveRight =>
          1.0

        case TurnClockwise | TurnCounterClockwise =>
          0.5

        case StartDrill =>
          0.1

        case AttachFastWheels =>
          return 0

        case _ =>
          1.0
      }
    }

    reversedActions.map(actionCost).sum
  }

  def totalTime: Int = reversedActions.size

  def isBooster(a : Action) : Boolean = {
    a match {
      case AttachFastWheels => true
      case StartDrill => true
      case _ => false
    }
  }

  def boostersCount() : Int = {
    reversedActions.filter(isBooster).size
  }
}
