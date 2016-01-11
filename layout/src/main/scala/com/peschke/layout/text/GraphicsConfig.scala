package com.peschke.layout.text

trait GraphicsConfig {
  def dot: Element
  def intersection: Element

  def innerFill(w: Int, h: Int): Element
  def outerFill(w: Int, h: Int): Element

  def northEastCorner: Element
  def northWestCorner: Element
  def southEastCorner: Element
  def southWestCorner: Element

  def northCorner: Element
  def eastCorner: Element
  def southCorner: Element
  def westCorner: Element

  def northHorizontalLine(l: Int): Element
  def southHorizontalLine(l: Int): Element
  def eastVerticalLine(l: Int): Element
  def westVerticalLine(l: Int): Element

  def horizontalLine(l: Int): Element
  def verticalLine(l: Int): Element
}

object GraphicsConfig {
  val Default = SimpleGraphicsConfig()
  val SharpCorners = ComplexGraphicsConfig(
    FillConfig(),
    CornerConfig(
      north = '┬',
      nEast = '┐',
      east  = '┤',
      sEast = '┘',
      south = '┴',
      sWest = '└',
      west  = '├',
      nWest = '┌',
      intersection = '┼'),
    LineConfig(
      north        = '─',
      east         = '│',
      south        = '─',
      west         = '│',
      vertical     = '│',
      horizontal   = '─',
      dot          = 'o'))
}

case class SimpleGraphicsConfig(
  fill: Char = ' ',
  corner: Char = '+',
  horizontal: Char = '-',
  vertical: Char = '|'
) extends GraphicsConfig {
  override lazy val dot: Element = Element(corner)
  override lazy val intersection: Element = Element(corner)

  override def innerFill(w: Int, h: Int): Element = Element(fill, w, h)
  override def outerFill(w: Int, h: Int): Element = innerFill(w, h)

  lazy private val cornerElement = Element(corner)
  override def northEastCorner: Element = cornerElement
  override def northWestCorner: Element = cornerElement
  override def southEastCorner: Element = cornerElement
  override def southWestCorner: Element = cornerElement

  override def northCorner: Element = cornerElement
  override def eastCorner: Element = cornerElement
  override def southCorner: Element = cornerElement
  override def westCorner: Element = cornerElement

  override def northHorizontalLine(l: Int): Element = horizontalLine(l)
  override def southHorizontalLine(l: Int): Element = horizontalLine(l)
  override def eastVerticalLine(l: Int): Element = verticalLine(l)
  override def westVerticalLine(l: Int): Element = verticalLine(l)

  override def horizontalLine(l: Int): Element = Element(horizontal, l, 1)
  override def verticalLine(l: Int): Element = Element(vertical, 1, l)
}

case class CornerConfig(
  north: Char = '+',
  nEast: Char = '+',
  east: Char = '+',
  sEast: Char = '+',
  south: Char = '+',
  sWest: Char = '+',
  west: Char = '+',
  nWest: Char = '+',
  intersection: Char = '+')

case class FillConfig(inner: Char = ' ', outer: Char = ' ')
case class LineConfig(
  north: Char = '-',
  east: Char = '|',
  south: Char = '-',
  west: Char = '|',
  vertical: Char = '|',
  horizontal: Char = '-',
  dot: Char = 'o')

case class ComplexGraphicsConfig(
  fill: FillConfig = FillConfig(),
  corners: CornerConfig = CornerConfig(),
  lines: LineConfig = LineConfig()) extends GraphicsConfig {

  override lazy val dot: Element = Element(lines.dot)
  override lazy val intersection: Element = Element(corners.intersection)

  override def innerFill(w: Int, h: Int): Element = Element(fill.inner, w, h)
  override def outerFill(w: Int, h: Int): Element = Element(fill.outer, w, h)

  override lazy val northEastCorner: Element = Element(corners.nEast)
  override lazy val northWestCorner: Element = Element(corners.nWest)
  override lazy val southEastCorner: Element = Element(corners.sEast)
  override lazy val southWestCorner: Element = Element(corners.sWest)

  override def northCorner: Element = Element(corners.north)
  override def eastCorner: Element = Element(corners.east)
  override def southCorner: Element = Element(corners.south)
  override def westCorner: Element = Element(corners.west)

  override def northHorizontalLine(l: Int): Element = Element(lines.north, l, 1)
  override def southHorizontalLine(l: Int): Element = Element(lines.south, l, 1)
  override def eastVerticalLine(l: Int): Element = Element(lines.east, 1, l)
  override def westVerticalLine(l: Int): Element = Element(lines.west, 1, l)

  override def horizontalLine(l: Int): Element = Element(lines.horizontal, l, 1)
  override def verticalLine(l: Int): Element = Element(lines.vertical, 1, l)
}
