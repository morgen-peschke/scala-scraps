package com.peschke.layout.text

import ElementComposer._

class BoxFactory(graphicsConfig: GraphicsConfig = GraphicsConfig.Default) {
  import graphicsConfig._

  implicit val settings = Settings(fill = ' ')

  def create(size: Int): Element = create(size, size)
  def create(w: Int, h: Int): Element = {
    def northLine = northHorizontalLine(w - 2)
    def southLine = southHorizontalLine(w - 2)

    def westLine = northWestCorner / westVerticalLine(h - 2) / southWestCorner
    def eastLine = northEastCorner / eastVerticalLine(h - 2) / southEastCorner

    lazy val middle = northLine / innerFill(w - 2, h - 2) / southLine

    if (w == 0 || h == 0) EmptyElement
    else if (w == 1 && h == 1) northCorner
    else if (w == 1) northCorner / verticalLine(h - 2) / southCorner
    else if (h == 1) westCorner ++ horizontalLine(w - 2) ++ eastCorner
    else if (middle.isEmpty) westLine ++ eastLine
    else westLine ++ middle ++ eastLine
  }
}

object BoxFactory {
  def apply(graphicsConfig: GraphicsConfig = GraphicsConfig.Default): BoxFactory =
    new BoxFactory(graphicsConfig)
}

object Box {
  def main(args: Array[String]) {
    val factory = BoxFactory(GraphicsConfig.SharpCorners)
    args
      .map(_.split(',').toList)
      .map({
        case w :: h :: xs => factory.create(w.toInt, h.toInt)
        case l :: Nil => factory.create(l.toInt)
        case Nil => EmptyElement
      })
      .map(_.render)
      .map(println)
  }
}
