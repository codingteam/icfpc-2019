package org.codingteam.icfpc2019

import java.awt.Color
import java.awt.image.BufferedImage

import org.codingteam.icfpc2019.spatialutils.{BitArray2D, Index2D, Index2DRange}

case class Task(map: TaskMap, startPos: Pos, obstacles: List[Obstacle], boosters: List[Booster]) {
  lazy val (range, filled, area) = {
    def checkedInt32(v: BigInt) = {
      require(v >= Int.MinValue && v <= Int.MaxValue, s"$v is too big for int")
      v.toInt
    }

    val boardVertices = map.vertices.toArray
    val boardXs = boardVertices.map(v => checkedInt32(v.x * 2))
    val boardYs = boardVertices.map(v => checkedInt32(v.y * 2))

    val minx = boardXs.min / 2
    val miny = boardYs.min / 2
    val maxx = boardXs.max / 2
    val maxy = boardYs.max / 2

    val boardStart = Index2D(minx, miny)
    val boardStop = Index2D(maxx, maxy)
    val boardSize = boardStop - boardStart
    //    println(s"start=$start stop=$stop")
    // 2 times greater.
    val img = new BufferedImage(boardSize.x * 2, boardSize.y * 2, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(Color.BLACK) // Non-free cells
    // Fill all area with non-free cells
    g.fillRect(0, 0, img.getWidth, img.getHeight)

    // Mark cells inside board as free
    g.setColor(Color.WHITE) // Background
    g.fillPolygon(boardXs map (v => v - minx * 2), boardYs map (v => v - miny * 2), boardXs.size)

    // Draw obstacles as non-free
    g.setColor(Color.BLACK) // Non-free cells
    for (obstacle <- obstacles) {
      val arr = obstacle.vertices.toArray
      val xs = arr.map(v => checkedInt32(v.x * 2))
      val ys = arr.map(v => checkedInt32(v.y * 2))

      g.fillPolygon(xs map (v => v - minx * 2), ys map (v => v - miny * 2), xs.size)
    }

    val matrix = new BitArray2D(boardSize)
    var area = 0
    for (ind <- matrix.indices) {
      // TODO: bulk read pixels (for speedup).
      val isFilled = img.getRGB(ind.x * 2 + 1, ind.y * 2 + 1) != Color.BLACK.getRGB()
      matrix(ind) = isFilled
      if (isFilled)
        area += 1
    }
    (Index2DRange(boardStart, boardStart + boardSize), matrix, area)
  }
}
