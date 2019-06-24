package org.codingteam.icfpc2019

import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

package object spatialutils {

  implicit class Vector2DExt(val p1: Vector2D) extends AnyVal {
    @inline def x = p1.getX

    @inline def y = p1.getY

    @inline def to(p2: Vector2D): Vector2D = new Vector2D(p2.x - p1.x, p2.y - p1.y)

    @inline def *(k: Double) = new Vector2D(k * x, k * y)

    @inline def +(p: Vector2D) = new Vector2D(x + p.x, y + p.y)

    @inline def -(p: Vector2D) = new Vector2D(x - p.x, y - p.y)

    @inline def *(p: Vector2D) = new Vector2D(x * p.x, y * p.y)

    @inline def /(p: Vector2D) = new Vector2D(x / p.x, y / p.y)
  }

}
