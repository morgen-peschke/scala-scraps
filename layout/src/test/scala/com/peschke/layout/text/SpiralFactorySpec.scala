package com.peschke.layout.text

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers

import ElementComposer._

class SpiralFactorySpec extends WordSpec with ShouldMatchers with ElementMatchers {
  import SpiralTestExtensions._
  import Direction._

  val factory = SpiralFactory()
  "create" should {
    "do nothing for a zero-sided spiral" in {
      factory.create(0).render should be ("")
    }

    "draw a dot for a spiral with width and height of 1" in {
      factory.create(1).render should be ("+")
    }

    "draw a spiral with the specified number of sides" in {
      factory.create(2).addRightBorder should renderTo(
        """|+-x
           |+ x""")
      factory.create(3).addRightBorder should renderTo(
        """|+--x
           ||  x
           |++ x""")
      factory.create(4).addRightBorder should renderTo(
        """|+---x
           ||   x
           || + x
           |+-+ x""")
    }

    "draw a spiral with the specified orientation" in {
      factory.create(4, East).addRightBorder should renderTo(
        """|+---x
           ||   x
           || + x
           |+-+ x""")
      factory.create(4, South).addRightBorder should renderTo(
        """|+--+x
           ||  |x
           |++ |x
           |   |x""")
      factory.create(4, West).addRightBorder should renderTo(
        """| +-+x
           | + |x
           |   |x
           |---+x""")
      factory.create(4, North).addRightBorder should renderTo(
        """||   x
           || ++x
           ||  |x
           |+--+x""")
    }

    "use the overridden characters" in {
      val factoryWithOverrides = SpiralFactory(ComplexGraphicsConfig(
        FillConfig(inner = '.', outer = 'o'),
        CornerConfig(
          north = '↑',
          nEast = '↗',
          east = '→',
          sEast = '↘',
          south = '↓',
          sWest = '↙',
          west = '←',
          nWest = '↖'),
        LineConfig(
          north = 'n',
          east = 'e',
          south = 's',
          west = 'w',
          vertical = 'v',
          horizontal = 'h')))

      factoryWithOverrides.create(4, East) should renderTo(
        """|↖hhh
           |v...
           |v.↗.
           |↙h↘.""")
      factoryWithOverrides.create(4, South) should renderTo(
        """|↖hh↗
           |v..v
           |↙↘.v
           |...v""")
      factoryWithOverrides.create(4, West) should renderTo(
        """|.↖h↗
           |.↙.v
           |...v
           |hhh↘""")
      factoryWithOverrides.create(4, North) should renderTo(
        """|v...
           |v.↖↗
           |v..v
           |↙hh↘""")
    }
  }
}

object SpiralTestExtensions {
  implicit val composerSettings: Settings = Settings()
  implicit class RichElement(val e: Element) extends AnyVal {
    def addRightBorder: Element = e ++ Element('x', 1, e.height)
  }
}
