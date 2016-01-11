package com.peschke.layout.text

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers
import Direction._
import ElementComposer._

class ElementComposerSpec extends WordSpec with ShouldMatchers with ElementMatchers {
  "ElementComposer" when {
    "padding" should {
      "add to the top with padNorth" in new UnAnchoredTest {
        smallBlock.padNorth(3) should renderTo(
          """|--
             |xx
             |xx""")

        smallBlock.padNorth(4) should renderTo(
          """|--
             |--
             |xx
             |xx""")
      }

      "add to the bottom with padSouth" in new UnAnchoredTest {
        smallBlock.padSouth(3) should renderTo(
          """|xx
             |xx
             |--""")

        smallBlock.padSouth(4) should renderTo(
          """|xx
             |xx
             |--
             |--""")
      }

      "add to the right with padEast" in new UnAnchoredTest {
        smallBlock.padEast(3) should renderTo(
          """|xx-
             |xx-""")

        smallBlock.padEast(4) should renderTo(
          """|xx--
             |xx--""")
      }

      "add to the right with padWest" in new UnAnchoredTest {
        smallBlock.padWest(3) should renderTo(
          """|-xx
             |-xx""")

        smallBlock.padWest(4) should renderTo(
          """|--xx
             |--xx""")
      }

      "widen centering horizontally with padWidth" in new UnAnchoredTest {
        smallBlock.padWidth(1) should renderTo(
          """|xx
             |xx""")

        smallBlock.padWidth(2) should renderTo(
          """|xx
             |xx""")

        smallBlock.padWidth(3) should renderTo(
          """|xx-
             |xx-""")

        smallBlock.padWidth(4) should renderTo(
          """|-xx-
             |-xx-""")
      }

      "heighten centering vertically with padHeight" in new UnAnchoredTest {
        smallBlock.padHeight(1) should renderTo(
          """|xx
             |xx""")

        smallBlock.padHeight(2) should renderTo(
          """|xx
             |xx""")


        smallBlock.padHeight(3) should renderTo(
          """|xx
             |xx
             |--""")

        smallBlock.padHeight(4) should renderTo(
          """|--
             |xx
             |xx
             |--""")
      }
    }

    "anchored North" should
    afterWord("grow vertically at the bottom, horizontally to the sides, in") {

      "extend" in new NorthAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|-xx--
             |-xx--
             |-----
             |-----
             |-----""")
      }

      "above" in new NorthAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|-xx-
             |-xx-
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |-xx-
             |-xx-""")
      }

      "beside" in new NorthAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|xxoooo
             |xxoooo
             |--oooo
             |--oooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|ooooxx
             |ooooxx
             |oooo--
             |oooo--""")
      }
    }

    "anchored NorthEast" should
    afterWord("grow vertically on the bottom, horizontally on the right, in") {

      "extend" in new NorthEastAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|---xx
             |---xx
             |-----
             |-----
             |-----""")
      }

      "above" in new NorthEastAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|--xx
             |--xx
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |--xx
             |--xx""")
      }

      "beside" in new NorthEastAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|xxoooo
             |xxoooo
             |--oooo
             |--oooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|ooooxx
             |ooooxx
             |oooo--
             |oooo--""")
      }
    }

    "anchored East" should
    afterWord("grow vertically on the sides, horizontally on the left, in") {

      "extend" in new EastAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|-----
             |---xx
             |---xx
             |-----
             |-----""")
      }

      "above" in new EastAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|--xx
             |--xx
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |--xx
             |--xx""")
      }

      "beside" in new EastAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|--oooo
             |xxoooo
             |xxoooo
             |--oooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|oooo--
             |ooooxx
             |ooooxx
             |oooo--""")
      }
    }

    "anchored SouthEast" should
    afterWord("grow vertically on the top, horizontally on the right, in") {

      "extend" in new SouthEastAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|-----
             |-----
             |-----
             |---xx
             |---xx""")
      }

      "above" in new SouthEastAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|--xx
             |--xx
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |--xx
             |--xx""")
      }

      "beside" in new SouthEastAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|--oooo
             |--oooo
             |xxoooo
             |xxoooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|oooo--
             |oooo--
             |ooooxx
             |ooooxx""")
      }
    }

    "anchored South" should
    afterWord("grow vertically at the bottom, horizontally to the sides, in") {

      "extend" in new SouthAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|-----
             |-----
             |-----
             |-xx--
             |-xx--""")
      }

      "above" in new SouthAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|-xx-
             |-xx-
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |-xx-
             |-xx-""")
      }

      "beside" in new SouthAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|--oooo
             |--oooo
             |xxoooo
             |xxoooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|oooo--
             |oooo--
             |ooooxx
             |ooooxx""")
      }
    }

    "anchored SouthWest" should
    afterWord("grow vertically on the top, horizontally on the left, in") {

      "extend" in new SouthWestAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|-----
             |-----
             |-----
             |xx---
             |xx---""")
      }

      "above" in new SouthWestAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|xx--
             |xx--
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |xx--
             |xx--""")
      }

      "beside" in new SouthWestAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|--oooo
             |--oooo
             |xxoooo
             |xxoooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|oooo--
             |oooo--
             |ooooxx
             |ooooxx""")
      }
    }

    "anchored West" should
    afterWord("grow vertically on the sides, horizontally on the right, in") {

      "extend" in new WestAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|-----
             |xx---
             |xx---
             |-----
             |-----""")
      }

      "above" in new WestAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|xx--
             |xx--
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |xx--
             |xx--""")
      }

      "beside" in new WestAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|--oooo
             |xxoooo
             |xxoooo
             |--oooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|oooo--
             |ooooxx
             |ooooxx
             |oooo--""")
      }
    }

    "anchored NorthWest" should
    afterWord("grow vertically on the bottom, horizontally on the left, in") {

      "extend" in new NorthWestAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|xx---
             |xx---
             |-----
             |-----
             |-----""")
      }

      "above" in new NorthWestAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|xx--
             |xx--
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |xx--
             |xx--""")
      }

      "beside" in new NorthWestAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|xxoooo
             |xxoooo
             |--oooo
             |--oooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|ooooxx
             |ooooxx
             |oooo--
             |oooo--""")
      }
    }

    "not anchored" should
    afterWord("grow and horizontally to the sides, in") {

      "extend" in new UnAnchoredTest {
        smallBlock.extend(5, 5) should renderTo(
          """|-----
             |-xx--
             |-xx--
             |-----
             |-----""")
      }

      "above" in new UnAnchoredTest {
        (smallBlock above largeBlock) should renderTo(
          """|-xx-
             |-xx-
             |oooo
             |oooo
             |oooo
             |oooo""")

        (largeBlock above smallBlock) should renderTo(
          """|oooo
             |oooo
             |oooo
             |oooo
             |-xx-
             |-xx-""")
      }

      "beside" in new UnAnchoredTest {
        (smallBlock beside largeBlock) should renderTo(
          """|--oooo
             |xxoooo
             |xxoooo
             |--oooo""")

        (largeBlock beside smallBlock) should renderTo(
          """|oooo--
             |ooooxx
             |ooooxx
             |oooo--""")
      }
    }
  }

  trait NorthAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(North))
  }

  trait NorthEastAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(NorthEast))
  }

  trait EastAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(East))
  }

  trait SouthEastAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(SouthEast))
  }

  trait SouthAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(South))
  }

  trait SouthWestAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(SouthWest))
  }

  trait WestAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(West))
  }

  trait NorthWestAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', Some(NorthWest))
  }

  trait UnAnchoredTest extends ElementComposerTest {
    implicit val composerSettings: Settings = Settings('-', None)
  }

  trait ElementComposerTest {
    val smallBlock = Element('x', 2, 2)
    val largeBlock = Element('o', 4, 4)
  }
}
