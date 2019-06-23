package org.codingteam.icfpc2019

import org.codingteam.icfpc2019.spatialutils.Index2D

case class Pos(x: BigInt, y: BigInt) {
  def toIndex2D: Index2D = {
    Index2D(x.toInt, y.toInt)
  }
  def isValid(): Boolean = x >= 0 && y >= 0

  def neighbours() : Set[Pos] = {
    Set(Pos(x-1,y), Pos(x+1,y), Pos(x,y-1), Pos(x,y+1))
  }

  override def toString: String = s"($x,$y)"
}
