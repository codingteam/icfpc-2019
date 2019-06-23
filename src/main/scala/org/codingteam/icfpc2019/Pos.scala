package org.codingteam.icfpc2019

import org.apache.commons.math3.geometry.euclidean.twod.Vector2D
import org.codingteam.icfpc2019.spatialutils.Index2D

case class Pos(x: BigInt, y: BigInt) {
  def toIndex2D: Index2D = {
    Index2D(x.toInt, y.toInt)
  }

  def toVector2D = new Vector2D(x.toInt, y.toInt)

  def isValid(): Boolean = x >= 0 && y >= 0

  def nearDistance(other: Pos): BigInt = (x - other.x).abs max (y - other.y).abs
}
