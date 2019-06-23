package main.scala.org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.Direction.Direction
import main.scala.org.codingteam.icfpc2019.Rotation.Rotation
import org.codingteam.icfpc2019.Pos

object Rotation extends Enumeration {
  type Rotation = Value
  val CLOCKWISE, COUNTERCLOCKWISE = Value
}

object Direction extends Enumeration {
  type Direction = Value
  val UP, LEFT, DOWN, RIGHT = Value

  def clockwise(v : Direction): Direction = {
    v match {
      case UP => RIGHT;
      case RIGHT => DOWN;
      case DOWN => LEFT;
      case LEFT => UP;
    }
  }

  def counterclockwise(v : Direction) : Direction = {
    v match {
      case UP => LEFT;
      case LEFT => DOWN;
      case DOWN => RIGHT;
      case RIGHT => UP;
    }
  }

  def rotate(dir : Direction, rot : Rotation) : Direction = {
    rot match {
      case Rotation.CLOCKWISE => clockwise(dir)
      case Rotation.COUNTERCLOCKWISE => counterclockwise(dir)
    }
  }

  def add(dir : Direction, rot : Int): Direction = {
    if (rot == 0)
      dir
    else if (rot == 1)
      counterclockwise(dir)
    else if (rot == -1)
      clockwise(dir)
    else if (rot > 1)
      add(dir, rot - 1)
    else
      add(dir, rot + 1)
  }

  // positive result: n COUNTERCLOCKWISE rotations
  // negative result: n CLOCKWISE rotations
  def diff(dir1 : Direction, dir2 : Direction) : Int = {
    (dir2.id - dir1.id) % 4
  }
}

case class Bot (position: Pos, direction : Direction, extraManipulators : Set[Pos]) {
  def rotation() : Int = {
    // We assume "default" direction is RIGHT
    Direction.diff(direction, Direction.RIGHT)
  }

  def rotatePos(pos : Pos, dir : Direction) : Pos = {
    // Rotate point from RIGHT to specified direction
    dir match {
      case Direction.RIGHT => pos
      case Direction.UP => Pos(-pos.y, pos.x)
      case Direction.LEFT => Pos(-pos.x, -pos.y)
      case Direction.DOWN => Pos(pos.y, -pos.x)
    }
  }

  // counterclockwise - positive
  def rotate(pos : Pos, rot : Int) : Pos = {
    if (rot == 0)
      pos
    else if (rot == 1)
      Pos(-pos.y, pos.x)
    else if (rot == -1)
      Pos(pos.y, -pos.x)
    else if (rot > 1)
      rotate(pos, rot - 1)
    else
      rotate(pos, rot + 1)
  }

  def translatePos(pos : Pos) : Pos = {
    Pos(position.x + pos.x, position.y + pos.y)
  }

  def makeRelativeRotation(pos : Pos) : Pos = {
    // Current direciton of the bot can be different than RIGHT;
    // we want to store relative positions in extraManipulators
    // val relative = Pos(pos.x - position.x, pos.y - position.y)
    rotate(pos, -rotation())
  }

  def makeRelative(pos : Pos) : Pos = {
    Pos(pos.x - position.x, pos.y - position.y)
  }

  private def baseCells() : Set[Pos] = {
    val x = position.x
    val y = position.y

    direction match {
      case Direction.RIGHT => Set(position, Pos(x+1,y), Pos(x+1, y+1), Pos(x+1, y-1)).filter(pos => pos.isValid())
      case Direction.UP => Set(position, Pos(x-1,y+1), Pos(x,y+1), Pos(x+1,y+1)).filter(pos => pos.isValid())
      case Direction.LEFT => Set(position, Pos(x-1,y-1), Pos(x-1,y), Pos(x-1,y+1)).filter(pos => pos.isValid())
      case Direction.DOWN => Set(position, Pos(x-1,y-1), Pos(x,y-1), Pos(x+1, y-1)).filter(pos => pos.isValid())
    }
  }

  private def absoluteExtraManipulators() : Set[Pos] = {
    extraManipulators.map(m => translatePos(rotate(m,rotation())))
  }

  def occupiedCells() : Set[Pos] = {
    baseCells() ++ absoluteExtraManipulators()
  }

  def boundingBox() : Set[Pos] = {
    val cells = occupiedCells()
    val xs = cells.map(_.x)
    val ys = cells.map(_.y)
    val minX = xs.min
    val minY = ys.min
    val maxX = xs.max
    val maxY = ys.max

    (for {x <- minX to maxX; y <- minY to maxY}
      yield Pos(x,y)).toSet
  }

  def evalLine(p1 : Pos, p2 : Pos, p : Pos) : Boolean = {
    def line(p : Pos) : BigInt = {
      (p.x - p1.x) * (p2.y - p1.y) - (p.y - p1.y) * (p2.x - p1.x)
    }
    if (p == p1 || p == p2)
      true
    else
      if (p1.x == p2.x) {
        p.x == p1.x
      } else if (p1.y == p2.y) {
        p.y == p1.y
      } else {
        line(p).abs <= 1
      }
  }

  def neighbours(board : Board) : Set[Pos] = {
    val cells = occupiedCells()
    cells.map(_.neighbours()).flatten.filter(p => board.isValidPosition(p) && ! cells.contains(p))
  }

  def isWrapped(board: Board, p: Pos) : Boolean = {
    if (!board.isValidPosition(p))
      false
    else {
      val obstacles = boundingBox().filter(x => !board.isValidPosition(x))
//      for (o <- obstacles) {
//        println(s"visible? $position => $p ($o): ${evalLine(position, p, o)}")
//      }
      obstacles.isEmpty || !obstacles.exists(o => evalLine(position, p, o))
    }
  }

  def wrappedCells(board : Board): Set[Pos] = {
    baseCells().filter(board.isValidPosition(_)) ++
      absoluteExtraManipulators().filter(isWrapped(board, _))
  }
}
