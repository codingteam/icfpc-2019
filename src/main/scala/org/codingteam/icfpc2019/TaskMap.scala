package org.codingteam.icfpc2019

case class TaskMap(vertices: List[Pos]) {
  val maxX = vertices.map(_.x).max

  val maxY = vertices.map(_.y).max
  val minX = vertices.map(_.x).min
  val minY = vertices.map(_.y).min

  def isValidPosition(pos : Pos): Boolean = {
    Obstacle(vertices).containsPosition(pos)
  }

  def size() : Pos = {
    Pos(maxX - minX, maxY - minY)
  }
}
