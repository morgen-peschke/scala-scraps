package com.peschke.math.l_system

import org.scalatest.{Matchers,WordSpec}
import CantorDust.{FILL,SPACE}

class CantorDustSpec extends WordSpec with Matchers {
  "CantorDust" should afterWord("produce at"){
    "n = 0 : FILL" in {
      CantorDust.generation(0).toSeq shouldBe Seq(FILL)
    }

    "n = 1 : FILL,SPACE,FILL" in {
      CantorDust.generation(1).toSeq shouldBe Seq(FILL,SPACE,FILL)
    }

    "n = 2 : FILL,SPACE,FILL,SPACE,SPACE,SPACE,FILL,SPACE,FILL" in {
      CantorDust.generation(2).toSeq shouldBe Seq(FILL,SPACE,FILL,SPACE,SPACE,SPACE,FILL,SPACE,FILL)
    }
  }

  "CantorDust.draw" should {
    "draw Cantor's fractal set" in {
      val cantor81 =
        """|---------------------------------------------------------------------------------
           |---------------------------                           ---------------------------
           |---------         ---------                           ---------         ---------
           |---   ---         ---   ---                           ---   ---         ---   ---
           |- -   - -         - -   - -                           - -   - -         - -   - -""".stripMargin
      CantorDust.draw(81) shouldBe cantor81
      CantorDust.draw(82) shouldBe cantor81
      CantorDust.draw(83) shouldBe cantor81
    }

    "respect the max generation limit" in {
      CantorDust.draw(27, Some(1)) shouldBe """|---------------------------""".stripMargin
      CantorDust.draw(27, Some(2)) shouldBe """|---------------------------
                                               |---------         ---------""".stripMargin
      CantorDust.draw(27, Some(3)) shouldBe """|---------------------------
                                               |---------         ---------
                                               |---   ---         ---   ---""".stripMargin
      CantorDust.draw(27, Some(4)) shouldBe """|---------------------------
                                               |---------         ---------
                                               |---   ---         ---   ---
                                               |- -   - -         - -   - -""".stripMargin
      CantorDust.draw(27, Some(5)) shouldBe """|---------------------------
                                               |---------         ---------
                                               |---   ---         ---   ---
                                               |- -   - -         - -   - -""".stripMargin
    }
  }
}
