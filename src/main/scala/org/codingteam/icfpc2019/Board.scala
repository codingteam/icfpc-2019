package main.scala.org.codingteam.icfpc2019

import java.awt.Color
import java.awt.image.BufferedImage

import org.codingteam.icfpc2019.spatialutils.{BitArray2D, Index2D, Index2DRange}
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
    println("there are " + area.toString() + " cells total, " + wrappedCells.size.toString() + " of which are wrapped")
    wrappedCells.size >= area
  }

  private lazy val (range, filled, area) = {
    def checkedInt32(v: BigInt) = {
      require(v >= Int.MinValue && v <= Int.MaxValue, s"$v is too big for int")
      v.toInt
    }

    val boardVertices = task.map.vertices.toArray
    val boardXs = boardVertices.map(v => checkedInt32(v.x * 2))
    val boardYs = boardVertices.map(v => checkedInt32(v.y * 2))

    val minx = boardXs.min / 2
    val miny = boardYs.min / 2
    val maxx = boardXs.max / 2
    val maxy = boardYs.max / 2

    val boardStart = Index2D(minx, miny)
    val boardStop = Index2D(maxx, maxy)
    val boardSize = boardStop - boardStart
    //    println(s"start=$start stop=$stop")
    // 2 times greater.
    val img = new BufferedImage(boardSize.x * 2, boardSize.y * 2, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(Color.BLACK) // Non-free cells
    // Fill all area with non-free cells
    g.fillRect(0, 0, img.getWidth, img.getHeight)

    // Mark cells inside board as free
    g.setColor(Color.WHITE) // Background
    g.fillPolygon(boardXs map (v => v - minx * 2), boardYs map (v => v - miny * 2), boardXs.size)

    // Draw obstacles as non-free
    g.setColor(Color.BLACK) // Non-free cells
    for (obstacle <- obstacles) {
      val arr = obstacle.vertices.toArray
      val xs = arr.map(v => checkedInt32(v.x * 2))
      val ys = arr.map(v => checkedInt32(v.y * 2))

      g.fillPolygon(xs map (v => v - minx * 2), ys map (v => v - miny * 2), xs.size)
    }

    val matrix = new BitArray2D(boardSize)
    var area = 0
    for (ind <- matrix.indices) {
      // TODO: bulk read pixels (for speedup).
      val isFilled = img.getRGB(ind.x * 2 + 1, ind.y * 2 + 1) != Color.BLACK.getRGB()
      matrix(ind) = isFilled
      if (isFilled)
        area += 1
    }
    (Index2DRange(boardStart, boardStart + boardSize), matrix, area)
  }

  def getArea() : Int = {
    area
  }

  def calcDistanceToUnwrapped(src : Pos) : Int = {
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
    var left = area - 1
    var isFreeCellMarked = false
    var prevFront = Set(src)
    matrix(src.x.intValue())(src.y.intValue()) = d
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
            }
          }
        }
      }

      prevFront = front
      //println(showMatrix())
      //println(s"Left: $left")
    } while (! isFreeCellMarked && left > 0)
    d
  }

  lazy val distanceToUnwrapped : Int = calcDistanceToUnwrapped(bot.position)

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