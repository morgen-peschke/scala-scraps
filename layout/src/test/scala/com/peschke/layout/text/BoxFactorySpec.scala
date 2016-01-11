package com.peschke.layout.text

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers

class BoxFactorySpec extends WordSpec with ShouldMatchers with ElementMatchers{
  val factory = BoxFactory()
  "create" should {
    "do nothing for a zero width/height box" in {
      factory.create(0, 0).render should be("")
      factory.create(5, 0).render should be("")
      factory.create(0, 5).render should be("")
    }

    "draw a dot for a box with width and height of 1" in {
      factory.create(1, 1).render should be("+")
    }

    "draw a line for a box with width or height of 1" in {
      factory.create(2, 1).render should be ("++")
      factory.create(3, 1).render should be ("+-+")
      factory.create(4, 1).render should be ("+--+")

      factory.create(1, 2) should renderTo(
        """|+
           |+""")
      factory.create(1, 3) should renderTo(
        """|+
           ||
           |+""")
      factory.create(1, 4) should renderTo(
        """|+
           ||
           ||
           |+""")
    }

    "draw a box with the specified with and height" in {
      factory.create(2, 2) should renderTo("""|++
                                              |++""")
      factory.create(3, 2) should renderTo("""|+-+
                                              |+-+""")
      factory.create(4, 2) should renderTo("""|+--+
                                              |+--+""")
      factory.create(2, 3) should renderTo("""|++
                                              |||
                                              |++""")
      factory.create(2, 4) should renderTo("""|++
                                              |||
                                              |||
                                              |++""")
      factory.create(3, 3) should renderTo("""|+-+
                                              || |
                                              |+-+""")
      factory.create(4, 4) should renderTo("""|+--+
                                              ||  |
                                              ||  |
                                              |+--+""")
    }

    "use the overridden characters" in {
      val factoryWithOverrides = BoxFactory(ComplexGraphicsConfig(
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

      factoryWithOverrides.create(1, 3) should renderTo("""|↑
                                                           |v
                                                           |↓""".stripMargin)

      factoryWithOverrides.create(3, 1).render shouldBe "←h→"

      factoryWithOverrides.create(3, 3) should renderTo("""|↖n↗
                                                           |w.e
                                                           |↙s↘""")
    }
  }
}
