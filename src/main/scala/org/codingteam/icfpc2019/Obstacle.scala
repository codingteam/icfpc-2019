package org.codingteam.icfpc2019

import java.awt.Color
import java.awt.image.BufferedImage

import org.codingteam.icfpc2019.spatialutils.{BitArray2D, Index2D, Index2DRange}

case class Obstacle(vertices: List[Pos]) {

  private lazy val (range, filled) = {
    def checkedInt32(v: BigInt) = {
      require(v >= Int.MinValue && v <= Int.MaxValue, s"$v is too big for int")
      v.toInt
    }

    val arr = vertices.toArray
    val xs = arr.map(v => checkedInt32(v.x * 2))
    val ys = arr.map(v => checkedInt32(v.y * 2))

    val minx = xs.min / 2
    val miny = ys.min / 2
    val maxx = xs.max / 2
    val maxy = ys.max / 2
    val start = Index2D(minx, miny)
    val stop = Index2D(maxx, maxy)
    //    println(s"start=$start stop=$stop")
    val size = stop - start
    // 2 times greater.
    val img = new BufferedImage(size.x * 2, size.y * 2, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(new Color(0x000000))
    g.fillRect(0, 0, img.getWidth, img.getHeight)
    g.setColor(new Color(0xffffff))
    //    println(xs.toSeq)
    //    println(ys.toSeq)
    g.fillPolygon(xs map (v => v - minx * 2), ys map (v => v - miny * 2), xs.size)

    val matrix = new BitArray2D(size)
    for (ind <- matrix.indices) {
      // TODO: bulk read pixels (for speedup).
      matrix(ind) = img.getRGB(ind.x * 2 + 1, ind.y * 2 + 1) != 0xff000000
      //      println(s"m($ind)=${matrix(ind)}")
    }
    (Index2DRange(start, start + size), matrix)
  }

  def containsPosition(pos: Pos): Boolean = {
    val ind = pos.toIndex2D
    (range contains ind) && filled(ind - range.a)
  }
}
