package com.peschke.algorithms.tree

import com.peschke.layout.text.{ Element, ElementComposer, Direction }

sealed trait Tree[+T] {
  def left: Tree[T]
  def right: Tree[T]
  def value: T
  def isEmpty: Boolean
  def render: String
  def toElement: Element
}

object Tree {
  def apply(): Tree[Nothing] = NilNode
  def apply[T](value: T): Tree[T] = ConsNode(value, NilNode, NilNode)
  def apply[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = ConsNode(value, left, right)
}

final case class ConsNode[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def isEmpty: Boolean = false
  def render: String = toElement.render

  def toElement: Element = (left, right) match {
    case (NilNode, NilNode) => Element(value.toString)
    case _ => toElementWithChildren
  }

  private def renderChildren: (Element, Element) = {
    import ElementComposer._
    import Direction._

    implicit val defaultSettings = Settings()
    val lineFill = Settings('|')
    val anchorSouthEast = Settings(anchor = Some(SouthEast))
    val anchorNorthEast = Settings(anchor = Some(NorthEast))

    val l = {
      val element = left.toElement
      val hLine = Element("-- ").padSouth(element.height / 2)
      val vLine = Element(".").padSouth(hLine.height)(lineFill)

      (vLine ++ hLine).beside(element)(anchorSouthEast)
    }

    val r = {
      val element = right.toElement
      val hLine = Element("-- ").padNorth(element.height / 2)
      val vLine = Element("'").padNorth(hLine.height)(lineFill)

      (vLine ++ hLine).beside(element)(Settings(anchor = Some(East)))
    }

    (l.padEast(r.width), r.padEast(l.width))
  }

  private def toElementWithChildren: Element = {
    import ElementComposer._
    import Direction._
    implicit val defaultSettings = Settings()

    val (leftSide, rightSide) = renderChildren

    val valueElement = {
      val bare = Element(value.toString).beside(Element(" -"))
      val withSide = bare.padEast(bare.width + 1)(Settings('|'))
      withSide.padEast(bare.width + leftSide.width)
    }

    val anchorEast = Settings(anchor = Some(East))


    leftSide.above(valueElement)(anchorEast).above(rightSide)(anchorEast)
  }

  override def toString: String = (left, right) match {
    case (NilNode, NilNode) => value.toString
    case _ => s"($value $left $right)"
  }
}

case object NilNode extends Tree[Nothing] {
  def left: Nothing = throw new TreeTraversalException("Accessing left node of NilNode")
  def right: Nothing = throw new TreeTraversalException("Accessing right node of NilNode")
  def value: Nothing = throw new TreeTraversalException("Accessing value of NilNode")
  def isEmpty: Boolean = true
  def render: String = "()"
  def toElement: Element = Element("()")
  override def toString: String = "_"
}

class TreeTraversalException(msg: String) extends RuntimeException(msg)
