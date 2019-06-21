package org.codingteam.icfpc2019

import org.codingteam.icfpc2019.spatialutils.Index2D

case class Pos(x: BigInt, y: BigInt) {
  def toIndex2D: Index2D = {
    Index2D(x.toInt, y.toInt)
  }
}
