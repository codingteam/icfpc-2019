package main.scala.org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.Direction.Direction
import main.scala.org.codingteam.icfpc2019.Rotation.Rotation
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D
import org.codingteam.icfpc2019.Pos
import org.codingteam.icfpc2019.spatialutils.{Index2D, Vector2DExt}

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
      add(add(dir,1), rot - 1)
    else
      add(add(dir,-1), rot + 1)
  }

  // positive result: n COUNTERCLOCKWISE rotations
  // negative result: n CLOCKWISE rotations
  def diff(dir1 : Direction, dir2 : Direction) : Int = {
    (dir1.id - dir2.id) % 4
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
      rotate(rotate(pos,1), rot - 1)
    else
      rotate(rotate(pos, -1), rot + 1)
  }

  def translatePos(pos : Pos) : Pos = {
    Pos(position.x + pos.x, position.y + pos.y)
  }

  def makeRelativeRotation(pos : Pos) : Pos = {
    // Current direciton of the bot can be different than RIGHT;
    // we want to store relative positions in extraManipulators
    // val relative = Pos(pos.x - position.x, pos.y - position.y)
    val result = rotate(pos, -rotation())
    println(s"rot: $pos (${(-rotation())}) => $result")
    result
  }

  def makeRelative(pos : Pos) : Pos = {
    val result = Pos(pos.x - position.x, pos.y - position.y)
    println(s"makeRelative: $pos wrt $position => $result")
    result
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
    extraManipulators.map(m => translatePos(rotatePos(m, direction)))
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

    val center = position.toVector2D

    def isVisibleFromCenter(p: Pos): Boolean = {
      (position nearDistance p).toInt match {
        case 0 => true
        case 1 => board isValidPosition p
        case _ =>
          if (!(board isValidPosition p))
            return false
          val last = p.toVector2D
          val dir = last subtract center

          val scaleX = dir.x.signum
          val scaleY = dir.y.signum
          val absDir = new Vector2D(dir.x.abs, dir.y.abs)
          val absDirAtan = math.atan2(absDir.y, absDir.x)

          def nextInt(x: Double) = {
            val a = math.ceil(x)
            if (a == x) x + 1 else a
          }

          def nextV(v: Vector2D, ind: Index2D): (Vector2D, Index2D) = {
            val corner = new Vector2D(nextInt(v.getX), nextInt(v.getY))
            val cornerDir = corner subtract v
            val cornerDirAtan = math.atan2(cornerDir.getY, cornerDir.getX)
            val Eps = 1e-12
            if (absDirAtan > cornerDirAtan + Eps) {
              // +y
              val x = v.x + dir.x / dir.y * (corner.y - v.y)
              (new Vector2D(x, corner.y), ind + Index2D(0, 1))
            } else if (absDirAtan < cornerDirAtan - Eps) {
              // +x
              val y = v.y + dir.y / dir.x * (corner.x - v.x)
              (new Vector2D(corner.x, y), ind + Index2D(1, 0))
            } else {
              // +x +y
              (corner, ind + Index2D.Ones)
            }
          }

          var (shiftV, shiftInd) = (new Vector2D(0.5, 0.5), Index2D.Zeros)
          val maxShift = Index2D(absDir.x.round.toInt, absDir.y.round.toInt)
          while (shiftInd.x < maxShift.x || shiftInd.y < maxShift.y) {
            val currentPos = Pos(position.x + shiftInd.x * scaleX, position.y + shiftInd.y * scaleY)
            if (!(board isValidPosition currentPos))
              return false
            val (v, ind) = nextV(shiftV, shiftInd)
            shiftV = v
            shiftInd = ind
          }
          true
      }
    }
    occupiedCells().filter(board.isValidPosition(_))
  }
}
