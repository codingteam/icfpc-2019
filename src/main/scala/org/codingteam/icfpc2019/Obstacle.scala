package org.codingteam.icfpc2019

import java.awt.{Color, Graphics2D, Transparency}
import java.awt.color.ColorSpace
import java.awt.geom.{GeneralPath, Path2D}
import java.awt.image.{BufferedImage, ComponentColorModel, DataBuffer, WritableRaster}

case class Obstacle(canvas : BufferedImage) {
  def containsPosition(pos : Pos) : Boolean = {
    // FIXME: not sure if everything is correct yet
    canvas.getRGB(pos.x.intValue, pos.y.intValue) != 0
  }
}

object Obstacle {
  def apply(vertices : List[Pos]) : Obstacle = {
    Obstacle(draw(vertices))
  }

  def draw(vertices : List[Pos]) : BufferedImage = {
    // var colorSpace : ColorSpace = ColorSpace.getInstance(ColorSpace.CS_GRAY)
    // var cm : ComponentColorModel = new ComponentColorModel(colorSpace, true, false, Transparency.OPAQUE, DataBuffer.TYPE_BYTE)
    // var wr : WritableRaster = cm.createCompatibleWritableRaster(100,100)
    var img : BufferedImage = new BufferedImage(100, 100)
    var g : Graphics2D = img.createGraphics()

    var polygon : Path2D = new Path2D.Double(vertices.length)
    polygon.moveTo(vertices.head.x.intValue(), vertices.head.y.intValue())
    for (vertex <- vertices.tail) {
      polygon.lineTo(vertex.x.doubleValue(), vertex.y.doubleValue())
    }
    polygon.closePath()
    g.setColor(Color.BLACK)
    g.fill(polygon)

    img
  }
}
