package main.scala.org.codingteam.icfpc2019

import org.codingteam.icfpc2019._

case class Board(task : Task, bot : Bot,
                 wrappedCells : Set[Pos],
                 obstacles : List[Obstacle],
                 remainingFastWheels : Int,
                 remainingDrill : Int,
                 solution: Solution
                ) {

  def isValidPosition(pos : Pos) : Boolean = {
    val ind = pos.toIndex2D
    (range contains ind) && filled(ind - range.a)
  }

  def calcFrontLength() : Int = {
    def isFront(pos : Pos) : Boolean = {
      val x = pos.x
      val y = pos.y
      val neighbours = for {dx <- -1 to 1; dy <- -1 to 1}
        yield Pos(x+dx, y+dy)
      neighbours.exists(p => isValidPosition(p) && ! wrappedCells.contains(p))
    }
    wrappedCells.filter(isFront).size
  }

  lazy val frontLength : Int = calcFrontLength()

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
  def isWrapped(detailedLogs: Boolean) : Boolean = {
    if (detailedLogs) {
      println("there are " + area.toString() + " cells total, " + wrappedCells.size.toString() + " of which are wrapped")
    }
    wrappedCells.size >= area
  }

  private def range = task.range
  private def filled = task.filled
  private def area = task.area

  def getArea() : Int = {
    area
  }

  def calcDistanceToUnwrapped(nearest : Boolean) : Int = {
    val size = task.map.size()
    val unmarked = -1
    val matrix = Array.fill[Int](size.x.intValue(), size.y.intValue())(unmarked)

    def showMatrix() : String = {
      var result = ""
      for (y <- 0 until size.y.intValue()) {
        for (x <- 0 until size.x.intValue()) {
          val d = matrix(x)(y)
          result = result + d.toString + " "
        }
        result = result + "\n"
      }
      result
    }

    var d : Int = 0
    var s : Int = 0
    var prevFront = bot.wrappedCells(this)
    var left = area - prevFront.size
    var isFreeCellMarked = false
    var stop = false
    for (pos <- prevFront)
      matrix(pos.x.intValue())(pos.y.intValue()) = d

    do {
      d += 1
      var front = Set[Pos]()
      for (cell <- prevFront) {
        val x = cell.x
        val y = cell.y
        val neighbours = List(Pos(x-1,y), Pos(x,y+1), Pos(x+1,y), Pos(x,y-1))
        for (neighbour <- neighbours) {
          val nx = neighbour.x
          val ny = neighbour.y
          if (isValidPosition(neighbour) && matrix(nx.intValue())(ny.intValue()) == unmarked) {
            matrix(nx.intValue())(ny.intValue()) = d
            front = front + neighbour
            //println(s"Mark: $neighbour")
            left -= 1
            if (! wrappedCells.contains(neighbour)) {
               isFreeCellMarked = true
               s += d
            }
          }
        }
      }

      prevFront = front
      if (nearest)
        stop = isFreeCellMarked
      else
        stop = false
      //println(showMatrix())
      //println(s"Left: $left")
    } while (! stop && left > 0)
    if (nearest)
      d
    else
      s
  }

  lazy val distanceToUnwrapped : Int = calcDistanceToUnwrapped(true)
  lazy val distanceToAllUnwrapped : Int = calcDistanceToUnwrapped(false)

  override def toString: String = {
    var result = new StringBuilder()
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
        result.append(point)
      }
      result.append("\n")
    }
    result + "\n" + solution.toString
  }
}

object Board {
  def apply(task : Task) : Board = {
    Board(task, Bot(task.startPos, Direction.RIGHT, Set[Pos]()), Set[Pos](), task.obstacles, 0, 0, new Solution(List[Action]()))
  }
}