package org.codingteam.icfpc2019.spatialutils

import scala.collection.AbstractIterable

/**
  * Index range.
  *
  * @param a lower bound (including).
  * @param b upper bound (excluding).
  */
case class Index2DRange(a: Index2D, b: Index2D) extends AbstractIterable[Index2D] {
  override def iterator: Iterator[Index2D] =
    for (x <- (a.x until b.x).toIterator;
         y <- (a.y until b.y).toIterator) yield Index2D(x, y)

  def contains(ind: Index2D): Boolean =
    ind.x >= a.x && ind.x < b.x && ind.y >= a.y && ind.y < b.y
}
