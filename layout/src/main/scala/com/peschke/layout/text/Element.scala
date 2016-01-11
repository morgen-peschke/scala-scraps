package com.peschke.layout.text

import Direction._

abstract class Element {
  def contents: Seq[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def render: String = contents mkString "\n"
  def isEmpty: Boolean = height == 0 || width == 0
  def nonEmpty: Boolean = !isEmpty

  def exists(p: Char => Boolean): Boolean = contents.exists(_.exists(p))

  def map(f: Char => Char): Element = Element(for (line <- contents) yield line map f)
}

object Element {
  def apply(): Element = EmptyElement
  def apply(line: String): Element = LineElement(line)
  def apply(lines: Seq[String], pad: Char = ' '): Element = ArrayElement(lines, pad)
  def apply(char: Char): Element = apply(char, 1, 1)
  def apply(fill: Char, w: Int, h: Int): Element = UniformElement(fill, w, h)
}

case object EmptyElement extends Element {
  override def contents = Seq("")
  override val height = 0
  override val width = 0
}

case class ArrayElement(lines: Seq[String], pad: Char) extends Element {

  private val elementContents: Array[String] = {
    val text = lines.mkString("\n").split("\n")
    if (text.nonEmpty) {
      val maxLength = text.map(_.length).max
      text.map(s => s + (pad.toString * (maxLength - s.length)))
    }
    else Array("")
  }

  override def contents: Seq[String] = elementContents.map(l => l)
}

case class LineElement(line: String) extends Element {

  require(!line.exists(_ == '\n'), "LineElement is a single line only")

  override def contents: Seq[String] = Seq(line)
  override val height: Int = 1
  override def width: Int = line.length
}

case class UniformElement(ch: Char, override val width: Int, override val height: Int)
    extends Element {

  require(width >= 0 && height >= 0, "negative dimensions are forbidden")

  private val elementContents: Array[String] = {
    val line = ch.toString * width
    Array.fill(height)(line)
  }

  override def contents: Seq[String] = elementContents.map(l => l)
}
