package org.codingteam.icfpc2019.spatialutils


/**
  * 2D array.
  *
  * @param size sizes.
  */
class Array2D[V: Manifest](val size: Index2D) extends Array2D.Common with Cloneable {
  private val values = Array.ofDim[V](size.x * size.y)

  def fill(v: V): Unit = {
    for (i <- values.indices)
      values(i) = v
  }

  def apply(ind: Index2D): V = {
    checkIndex(ind)
    values(getIndex(ind))
  }

  def update(ind: Index2D, v: V): Unit = {
    checkIndex(ind)
    values(getIndex(ind)) = v
  }

  def map[R: Manifest](f: V => R): Array2D[R] = {
    val result = new Array2D[R](size)
    for (i <- values.indices)
      result.values(i) = f(values(i))
    result
  }

  def zipmap[R: Manifest, V2](other: Array2D[V2])(f: (V, V2) => R): Array2D[R] = {
    val result = new Array2D[R](this.size min other.size)
    for (ind <- result.indices)
      result(ind) = f(this (ind), other(ind))
    result
  }

  def asRows = values.grouped(size.x)

  def valuesIterator = values.iterator

  def slice(range: Index2DRange): Array2D[V] = {
    val size = range.b - range.a + Index2D.Ones
    val res = Array2D[V](size)
    for (ind <- res.size.indicesFromZero)
      res(ind) = this (ind + range.a)
    res
  }

  override def clone(): Array2D[V] = {
    val other = new Array2D[V](size)
    System.arraycopy(values, 0, other.values, 0, values.length)
    other
  }
}

object Array2D {

  trait Common {
    def size: Index2D

    def indices: Iterator[Index2D] =
      for (y <- (0 until size.y).iterator;
           x <- (0 until size.x).iterator) yield
        Index2D(x, y)

    @inline protected final def getIndex(ind: Index2D): Int = ind.y * size.x + ind.x

    @inline protected final def checkIndex(ind: Index2D): Unit = {
      assert(ind.x >= 0 && ind.x < size.x)
      assert(ind.y >= 0 && ind.y < size.y)
    }

  }

  def apply[T: Manifest](size: Index2D): Array2D[T] = new Array2D[T](size)

  def apply[T: Manifest](size: Index2D, values: Array[T]): Array2D[T] = {
    require(size.x * size.y == values.length, "Wrong array size")
    val res = new Array2D[T](size)
    System.arraycopy(values, 0, res.values, 0, values.length)
    res
  }

  def apply[T: Manifest](values: Array[Array[T]]): Array2D[T] = {
    val sizeY = values.size
    if (sizeY == 0)
      return Array2D[T](Index2D.Zeros)
    val sizeX = values(0).size
    require(values.forall(_.size == sizeX), "Different row sizes")
    val res = new Array2D[T](Index2D(sizeX, sizeY))
    if (sizeX == 0)
      return res
    for (y <- 0 until sizeY)
      System.arraycopy(values(y), 0, res.values, y * sizeX, sizeX)
    res
  }

}
