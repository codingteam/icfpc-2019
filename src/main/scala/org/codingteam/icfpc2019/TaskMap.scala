package org.codingteam.icfpc2019

case class TaskMap(vertices: List[Pos]) {
  val maxX = vertices.map(_.x).max

  val maxY = vertices.map(_.y).max

  def isValidPosition(pos : Pos): Boolean = {
    val xs = vertices.map(_.x)
    val ys = vertices.map(_.y)
    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max

    (pos.x >= minX) && (pos.x < maxX) && (pos.y >= minY) && (pos.y < maxY)
  }
}
