package org.codingteam.icfpc2019.spatialutils

/** 2D-index of point. */
case class Index2D(x: Int, y: Int) {

  def distanceTo(other: Index2D): Double = math.hypot(x - other.x, y - other.y)

  def min(other: Index2D): Index2D = Index2D(x min other.x, y min other.y)

  def max(other: Index2D): Index2D = Index2D(x max other.x, y max other.y)

  def -(other: Index2D): Index2D = Index2D(x - other.x, y - other.y)

  def +(other: Index2D): Index2D = Index2D(x + other.x, y + other.y)

  def nearIndices(upperBound: Index2D): IndexedSeq[Index2D] = {
    for (yy <- math.max(y - 1, 0) to math.min(y + 1, upperBound.y - 1);
         xx <- math.max(x - 1, 0) to math.min(x + 1, upperBound.x - 1)
         if !(xx == x && yy == y)) yield
      Index2D(xx, yy)
  }

  def indicesFromZero: Iterator[Index2D] =
    for (j <- (0 until y).iterator;
         i <- (0 until x).iterator) yield Index2D(i, j)

}

object Index2D {
  lazy val Zeros = Index2D(0, 0)
  lazy val Ones = Index2D(1, 1)
}