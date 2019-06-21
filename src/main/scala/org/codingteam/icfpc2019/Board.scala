package main.scala.org.codingteam.icfpc2019

import org.codingteam.icfpc2019.{Obstacle, Pos, Solution, Task}

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
    assert(task.obstacles.isEmpty)

    val bottomLeftX = task.map.vertices.map(pos => pos.x).min
    val bottomLeftY = task.map.vertices.map(pos => pos.y).min
    val topRightX = task.map.vertices.map(pos => pos.x).max
    val topRightY = task.map.vertices.map(pos => pos.y).max
    val cellsCount = (topRightX - bottomLeftX) * (topRightY - bottomLeftY)

    println("there are " + cellsCount.toString() + " cells total, " + wrappedCells.size.toString() + " of which are wrapped")
    wrappedCells.size >= cellsCount
  }
}
