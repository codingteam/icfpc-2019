package main.scala.org.codingteam.icfpc2019

import org.codingteam.icfpc2019._

case class Board(task : Task, bot : Bot,
                 wrappedCells : Set[Pos],
                 obstacles : List[Obstacle],
                 boosters: Set[Booster],
                 extraManipulators : Int,
                 hasFastWheels : Boolean,
                 fastWheelsEnabled : Boolean,
                 remainingFastWheels : Int,
                 remainingDrills: Int,
                 remainingDrillTicks : Int,
                 drilledObstacles : Set[Pos],
                 solution: Solution,
                 // How many teleportation boosters we collected?
                 teleportsCount: Int,
                 installedTeleports: Vector[Pos]
                ) {

  def isValidPosition(pos : Pos) : Boolean = {
    val ind = pos.toIndex2D
    (range contains ind) && (filled(ind - range.a) || drilledObstacles.contains(pos))
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
    // Wheels
    val newWheels = if (fastWheelsEnabled && remainingFastWheels >= 1) remainingFastWheels - 1 else 0
    val newWheelsEnabled = if (fastWheelsEnabled && remainingFastWheels == 0) false else fastWheelsEnabled
    val newHasFastWheels = if (isAtFastWheelsBooster())
                             true
                           else
                             if (remainingFastWheels > 0)
                                hasFastWheels
                             else false

    // Drills
    val newDrills = currentBooster() match {
      case Some(Drill(_)) => remainingDrills + 1
      case _ => remainingDrills
    }
    val newDrill = if (remainingDrillTicks >= 1) remainingDrillTicks - 1 else 0

    // Teleports
    val newTeleportsCount = teleportsCount + (currentBooster() match {
      case Some(Teleport(_)) => 1
      case _ => 0
    })

    // Extra manipulators
    val newManipulators = currentBooster() match {
      case Some(ManipulatorExtension(_)) => extraManipulators + 1
      case _ => extraManipulators
    }

    val newBoosters = currentBooster() match {
      case None => boosters
      case Some(booster) => boosters.filter(b => b != booster)
    }

    //println("Booster: " + currentBooster().toString)
    copy(boosters = newBoosters,
      extraManipulators = newManipulators,
      remainingFastWheels = newWheels,
      remainingDrills = newDrills,
      remainingDrillTicks = newDrill,
      fastWheelsEnabled = newWheelsEnabled,
      hasFastWheels = newHasFastWheels,
      teleportsCount = newTeleportsCount)
  }

  def isDrillEnabled() : Boolean = remainingDrillTicks > 0

  def isFastWheelsEnabled() : Boolean = fastWheelsEnabled && remainingFastWheels > 0

  def currentBooster() : Option[Booster] = {
    boosters.filter(b => bot.position == b.pos).toList match {
      case Nil => None
      case booster :: _ => Some(booster)
    }
  }

  def boosterAt(pos: Pos) : Option[Booster] = {
    boosters.filter(b => b.pos == pos).toList match {
      case Nil => None
      case booster :: _ => Some(booster)
    }
  }

  def isAtFastWheelsBooster() : Boolean = {
    currentBooster() match {
      case Some(FastWheels(_)) => true
      case _ => false
    }
  }

  // TODO[M]: Replace with a full-fledged check. For now, I assume there are no obstacles
  def isWrapped(detailedLogs: Boolean) : Boolean = {
    if (detailedLogs) {
      println("there are " + getArea.toString() + " cells total, " + wrappedCells.size.toString() + " of which are wrapped")
    }
    wrappedCells.size >= getArea
  }

  private def range = task.range
  private def filled = task.filled
  private def area = task.area

  def getArea() : Int = {
    area + drilledObstacles.size
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
          result = result + f"$d%2d" + " "
        }
        result = result + "\n"
      }
      result
    }

    var d : Int = 0
    var s : Int = 0
    var prevFront = Set[Pos](bot.position)
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

//    if (left == 0) {
//      d = 0
//      s = 0
//    }

    if (nearest)
      d
    else
      s
  }

  lazy val distanceToUnwrapped : Int = calcDistanceToUnwrapped(true)
  lazy val distanceToAllUnwrapped : Int = calcDistanceToUnwrapped(false)

  override def toString: String = {
    var result = new StringBuilder()
    for (y <- task.map.maxY to task.map.minY by -1) {
      for (x <- task.map.minX to task.map.maxX) {
        val pos = Pos(x,y)
        val point =
          if (pos == bot.position)
            "o"
          else
            if (bot.wrappedCells(this).contains(pos))
              "∘"
            else
              boosterAt(pos) match {
                case None =>
                  if (isValidPosition(pos))
                    if (wrappedCells.contains(pos))
                      "+"
                    else
                      " "
                  else "#"
                case Some(booster) => booster.symbol()
              }
        result.append(point)
      }
      result.append("\n")
    }

    val drills = remainingDrillTicks.toString + " drill ticks, and " + remainingDrills + " drills, remain"

    val aboutBoosters = boosters.mkString(", ") +
      s"\nhas fast wheels: $hasFastWheels, fast wheels enabled: $fastWheelsEnabled, remaining: $remainingFastWheels" +
      s"\nteleports count: $teleportsCount, installed at: $installedTeleports"
    result + "\n" + aboutBoosters + "\n" + drills + "\n" + solution.toString
  }

  def withoutSolution(): Board = {
    copy(solution = new Solution(Vector[Action]()))
  }
}

object Board {
  def apply(task : Task) : Board = {
    Board(task,
      Bot(task.startPos, Direction.RIGHT, Set[Pos]()),
      Set[Pos](),
      task.obstacles,
      task.boosters.toSet,
      0,
      false, false,
      0, 0, 0,
      Set[Pos](),
      new Solution(Vector[Action]()),
      0,
      Vector[Pos]())
  }
}