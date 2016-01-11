package com.peschke.math.l_system

import org.scalatest.{Matchers,WordSpec}

class LSystemSpec extends WordSpec with Matchers {
  "LSystem.floorLog3" should {
    "return 1 for zero or a negative number" in {
      LSystem.floorLog3( 0) shouldBe 1
      LSystem.floorLog3(-1) shouldBe 1
    }

    "return the largest power of three that is less than, or equal to, the input" in {
      LSystem.floorLog3(2) shouldBe 1
      LSystem.floorLog3(3) shouldBe 3
      LSystem.floorLog3(4) shouldBe 3
      LSystem.floorLog3(8) shouldBe 3
      LSystem.floorLog3(9) shouldBe 9
      LSystem.floorLog3(10) shouldBe 9
      LSystem.floorLog3(26) shouldBe 9
      LSystem.floorLog3(27) shouldBe 27
      LSystem.floorLog3(28) shouldBe 27
      LSystem.floorLog3(80) shouldBe 27
      LSystem.floorLog3(81) shouldBe 81
    }
  }
}
