package com.peschke.layout.text

import Direction._

object ElementComposer {

  case class Settings(fill: Char = ' ', anchor: Option[Direction] = None)

  implicit class ComposableElement(val elem: Element) extends AnyVal {
    def ++ (that: Element)(implicit s: Settings): Element = elem beside that
    def /  (that: Element)(implicit s: Settings): Element = elem above that

    def above(that: Element)(implicit s: Settings): Element = {
      val t = elem extend (that.width, elem.height)
      val b = that extend (elem.width, that.height)
      Element(t.contents ++ b.contents)
    }

    def beside(that: Element)(implicit s: Settings): Element = {
      val l = elem extend (elem.width, that.height)
      val r = that extend (that.width, elem.height)
      Element(for( (a,b) <- l.contents zip r.contents) yield a + b)
    }

    def extend(w: Int, h: Int)(implicit s: Settings): Element =
      s.anchor match {
        case Some(North)     => elem padSouth(h)  padWidth (w)
        case Some(NorthEast) => elem padSouth(h)  padWest  (w)
        case Some(East)      => elem padHeight(h) padWest  (w)
        case Some(SouthEast) => elem padNorth(h)  padWest  (w)
        case Some(South)     => elem padNorth(h)  padWidth (w)
        case Some(SouthWest) => elem padNorth(h)  padEast  (w)
        case Some(West)      => elem padHeight(h) padEast  (w)
        case Some(NorthWest) => elem padSouth(h)  padEast  (w)
        case None            => elem padHeight(h) padWidth (w)
      }

    def padNorth(h: Int)(implicit s: Settings): Element =
      if (h <= elem.height) elem
      else Element(s.fill, elem.width, h - elem.height) above elem

    def padSouth(h: Int)(implicit s: Settings): Element =
      if (h <= elem.height) elem
      else elem above (Element(s.fill, elem.width, h - elem.height))

    def padEast(w: Int)(implicit s: Settings): Element =
      if (w <= elem.width) elem
      else elem beside Element(s.fill, w - elem.width, elem.height)

    def padWest(w: Int)(implicit s: Settings): Element =
      if (w <= elem.width) elem
      else Element(s.fill, w - elem.width, elem.height) beside elem

    def padHeight(h: Int)(implicit s: Settings): Element =
      if (h <= elem.height) elem
      else Element(s.fill, elem.width, (h - elem.height)/2) above elem padSouth h

    def padWidth(w: Int)(implicit s: Settings): Element =
      if (w <= elem.width) elem
      else Element(s.fill, (w - elem.width)/2, elem.height) beside elem padEast w

    def boxed(implicit s: Settings): Element = elem.extend(elem.width + 2, elem.height + 2)
  }
}
