package com.peschke.graphics.utils

import _root_.scalafx.scene.canvas.GraphicsContext

case class Turtle(x: Double, y: Double, heading: Double = 0) {
  def move(distance: Double): Turtle = copy(
    x = x + (distance * Math.cos(Math.toRadians(heading % 360))),
    y = y + (distance * Math.sin(Math.toRadians(heading % 360))))

  def rotate(delta: Double): Turtle = copy(heading = (heading + delta) % 360)

  def line(distance: Double)(gc: => GraphicsContext): Turtle = {
    val end = move(distance)
    gc.strokeLine(x, y, end.x, end.y)
    end
  }
}
