package org.codingteam.icfpc2019.spatialutils

import java.util

class BitArray2D private(val size: Index2D,
                         private val values: Array[Long]) extends Array2D.Common with Cloneable {

  def this(size: Index2D) = this(size, new Array[Long]((size.x * size.y + 63) >> 6 max 1))

  def fill(v: Boolean): Unit = {
    util.Arrays.fill(values, if (v) ~0L else 0L)
  }

  def apply(inds: Index2D): Boolean = {
    checkIndex(inds)
    val ind = getIndex(inds)
    val i = ind >> 6
    val mask = 1L << (ind & 63)
    (values(i) & mask) != 0
  }

  def update(inds: Index2D, v: Boolean): Unit = {
    checkIndex(inds)
    val ind = getIndex(inds)
    val i = ind >> 6
    val mask = 1L << (ind & 63)
    if (v)
      values(i) |= mask
    else
      values(i) &= ~mask
  }

  override def clone(): BitArray2D = {
    new BitArray2D(size, util.Arrays.copyOf(values, values.length))
  }
}
