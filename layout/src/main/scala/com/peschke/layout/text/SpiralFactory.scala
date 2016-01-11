package com.peschke.layout.text

import ElementComposer._

class SpiralFactory(graphicsConfig: GraphicsConfig = GraphicsConfig.Default) {
  import graphicsConfig._
  import Direction._

  implicit val settings = Settings(fill = ' ')

  def create(edgeCount: Int, direction: Direction = East): Element = {
    if (edgeCount == 0) EmptyElement
    else if (edgeCount == 1) direction match {
      case North | NorthEast => southWestCorner
      case East  | SouthEast => northWestCorner
      case South | SouthWest => northEastCorner
      case West  | NorthWest => southEastCorner
    }
    else {
      val sp = create(edgeCount - 1, direction.prev.prev)
      def verticalBar = verticalLine(sp.height)
      def horizontalBar = horizontalLine(sp.width)
      def verticalSpace = innerFill(1, sp.height)
      def horizontalSpace = innerFill(sp.width, 1)
      direction match {
        case North | NorthEast => (verticalBar / southWestCorner) ++ (horizontalSpace / sp)
        case East  | SouthEast => (northWestCorner ++ horizontalBar) / (sp ++ verticalSpace)
        case South | SouthWest => (sp / horizontalSpace) ++ (northEastCorner / verticalBar)
        case West  | NorthWest => (verticalSpace ++ sp) / (horizontalBar ++ southEastCorner)
      }
    }
  }
}

object SpiralFactory {
  def apply(graphicsConfig: GraphicsConfig = GraphicsConfig.Default): SpiralFactory =
    new SpiralFactory(graphicsConfig)
}

object Spiral {
  def main(args: Array[String]) {
    val factory = SpiralFactory(GraphicsConfig.SharpCorners)
    println((args.toList match {
      case sides :: direction :: xs => factory.create(sides.toInt, Direction(direction))
      case sides :: Nil             => factory.create(sides.toInt, Direction.East)
      case Nil                      => EmptyElement
    }).render)
  }
}
