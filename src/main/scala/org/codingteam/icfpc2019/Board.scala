package main.scala.org.codingteam.icfpc2019

import org.codingteam.icfpc2019.{Action, Obstacle, Pos, Solution, Task}

case class Board(task : Task, bot : Bot,
                 wrappedCells : Set[Pos],
                 obstacles : List[Obstacle],
                 remainingFastWheels : Int,
                 remainingDrill : Int,
                 solution: Solution
                ) {

  def isValidPosition(pos : Pos) : Boolean = {
    task.map.isValidPosition(pos) && ! obstacles.map(_.containsPosition(pos)).exists(identity)
  }

  def isValid() : Boolean = {
    isValidPosition(bot.position)
  }

  def tick() : Board = {
    val newWheels = if (remainingFastWheels >= 1) remainingFastWheels - 1 else 0
    val newDrill = if (remainingDrill >= 1) remainingDrill - 1 else 0
    copy(remainingFastWheels = newWheels, remainingDrill = newDrill)
  }

  def isDrillEnabled() : Boolean = remainingDrill > 0

  def isFastWheelsEnabled() : Boolean = remainingFastWheels > 0

  // TODO[M]: Replace with a full-fledged check. For now, I assume there are no obstacles
  def isWrapped() : Boolean = {
    val boardArea = Obstacle(task.map.vertices).getArea
    val obstaclesArea = task.obstacles.map(_.getArea).sum
    val cellsCount = boardArea - obstaclesArea
    println("there are " + cellsCount.toString() + " cells total, " + wrappedCells.size.toString() + " of which are wrapped")
    wrappedCells.size >= cellsCount
  }

  override def toString: String = {
    var result : String = ""
    for (y <- task.map.minY to task.map.maxY) {
      for (x <- task.map.minX to task.map.maxX) {
        val pos = Pos(x,y)
        val point =
          if (pos == bot.position)
            "o"
          else
            if (bot.wrappedCells(this).contains(pos))
              "∘"
            else
              if (isValidPosition(pos))
                  if (wrappedCells.contains(pos))
                    "+"
                  else
                    " "
                else "#"
        result = result + point
      }
      result = result + "\n"
    }
    result + "\n" + solution.toString
  }
}

object Board {
  def apply(task : Task) : Board = {
    Board(task, Bot(task.startPos, Direction.RIGHT, Set[Pos]()), Set[Pos](), task.obstacles, 0, 0, new Solution(List[Action]()))
  }
}