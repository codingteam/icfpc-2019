package main.scala.org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.Direction.Direction
import org.codingteam.icfpc2019.Pos

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
}

case class Bot (position: Pos, direction : Direction) {
  def occupiedCells() : Set[Pos] = {
    val x = position.x
    val y = position.y
    direction match {
      case Direction.RIGHT => Set(position, Pos(x+1,y), Pos(x+1, y+1), Pos(x+1, y-1))
      case Direction.UP => Set(position, Pos(x-1,y+1), Pos(x,y+1), Pos(x+1,y+1))
      case Direction.LEFT => Set(position, Pos(x-1,y-1), Pos(x-1,y), Pos(x-1,y+1))
      case Direction.DOWN => Set(position, Pos(x-1,y-1), Pos(x,y-1), Pos(x+1, y-1))
    }
  }
}
