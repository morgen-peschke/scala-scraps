package com.peschke.math.l_system

import org.scalatest.{Matchers,WordSpec}
import SquareKochCurve.{F,L,R}

class SquareKochCurveSpec extends WordSpec with Matchers {
  "SquareKochCurve" should afterWord("produce at"){
    "n = 0 : F" in {
      SquareKochCurve.generation(0).toSeq shouldBe Seq(F)
    }

    "n = 1 : F+F−F−F+F" in {
      SquareKochCurve.generation(1).toSeq shouldBe Seq(F,L,F,R,F,R,F,L,F)
    }

    "n = 2 : F+F−F−F+F + F+F−F−F+F − F+F−F−F+F − F+F−F−F+F + F+F−F−F+F" in {
      SquareKochCurve.generation(2).toSeq shouldBe Seq(
        F,L,F,R,F,R,F,L,F, L,
        F,L,F,R,F,R,F,L,F, R,
        F,L,F,R,F,R,F,L,F, R,
        F,L,F,R,F,R,F,L,F, L,
        F,L,F,R,F,R,F,L,F)
    }
  }

  "SquareKochCurve.draw" should {
    "draw the square variant of the Koch curve" in {
      SquareKochCurve.draw(0, Some(1)) shouldBe "─"

      SquareKochCurve.draw(1, Some(1)) shouldBe Seq(
        " ┌─┐ ",
        " | | ",
        "─┘ └─").mkString("\n")

      SquareKochCurve.draw(2, Some(1)) shouldBe Seq(
        "       ┌─┐       ",
        "       | |       ",
        "     ┌─┘ └─┐     ",
        "     |     |     ",
        "   ┌─┘     └─┐   ",
        "   |         |   ",
        " ┌─┼─┐     ┌─┼─┐ ",
        " | | |     | | | ",
        "─┘ └─┘     └─┘ └─").mkString("\n")
    }
  }
}
