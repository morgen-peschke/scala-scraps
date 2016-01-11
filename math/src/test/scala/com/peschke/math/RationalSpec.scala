package com.peschke.math

import org.scalatest.WordSpec
import org.scalatest.Matchers

class RationalSpec extends WordSpec with Matchers {

  def knowThat = afterWord("know that")

  "Rational" when {
    "constructed with a zero denominator" should {
      "throw an exception" in {
        intercept[IllegalArgumentException] {
          Rational(0,0)
        }.getMessage should be("requirement failed: denominator cannot be 0")
      }
    }

    "constructed from a float" should {
      "handle easily rational-able numbers" in {
        Rational(0.5) should be (Rational(1,2))
        Rational(0.8) should be (Rational(4,5))
        Rational(0.25) should be (Rational(1,4))
      }

      "approximate irrational numbers" in {
        Rational(scala.math.Pi) should be (Rational(289,92))
        Rational(1/3) should be (Rational(1/3))
        Rational(2/3) should be (Rational(2/3))
        Rational(1/10) should be (Rational(1/10))
      }
    }

    "comparing with '<'" should knowThat {
      "1/2 < 1/3 is false" in ((Rational(1,2) < Rational(1,3)) should be (false))
      "1/3 < 1/2 is true"  in ((Rational(1,3) < Rational(1,2)) should be (true))
      "1/2 < 1/2 is false" in ((Rational(1,2) < Rational(1,2)) should be (false))
    }

    "adding two rationals" should knowThat {
      "1/2 + 1/3 is 5/6" in ((Rational(1,2) + Rational(1,3)) should be (Rational(5,6)))
      "1/3 + 1/2 is 5/6" in ((Rational(1,3) + Rational(1,2)) should be (Rational(5,6)))
    }

    "subtracting two rationals" should knowThat {
      "5/6 - 1/3 is 1/2"  in ((Rational(5,6) - Rational(1,3)) should be (Rational(1,2)))
      "5/6 - 1/2 is 1/3"  in ((Rational(5,6) - Rational(1,2)) should be (Rational(1,3)))
      "1/3 - 5/6 is -1/2" in ((Rational(1,3) - Rational(5,6)) should be (Rational(-1,2)))
      "1/2 - 5/6 is -1/3" in ((Rational(1,2) - Rational(5,6)) should be (Rational(-1,3)))
      "1/2 - 1/2 is 0"    in ((Rational(1,2) - Rational(1,2)) should be (Rational(0,1)))
    }

    "multiplying two rationals" should knowThat {
      "1/2 * 1/3 is 1/6" in ((Rational(1,2) * Rational(1,3)) should be (Rational(1,6)))
      "1/3 * 1/2 is 1/6" in ((Rational(1,3) * Rational(1,2)) should be (Rational(1,6)))
      "1/2 * 2/1 is 1"   in ((Rational(1,2) * Rational(2,1)) should be (Rational(1)))
      "1/3 * 2/1 is 2/3" in ((Rational(1,3) * Rational(2,1)) should be (Rational(2,3)))
    }

    "dividing two rationals" should knowThat {
      "1/2 / 1/3 is 3/2" in ((Rational(1,2) / Rational(1,3)) should be (Rational(3,2)))
      "1/3 / 1/2 is 2/3" in ((Rational(1,3) / Rational(1,2)) should be (Rational(2,3)))
      "1/2 / 2 is 1/4" in ((Rational(1,2) / Rational(2)) should be (Rational(1,4)))
      "1/3 / 2 is 1/6" in ((Rational(1,3) / Rational(2)) should be (Rational(1,6)))
      "2 / 1/2 is 4" in ((Rational(2) / Rational(1,2)) should be (Rational(4)))
      "2 / 1/3 is 6" in ((Rational(2) / Rational(1,3)) should be (Rational(6)))
    }
    "case matching" should {
      "recover the numerator and denominator" in {
        Rational(1,2) match {
          case Rational(n: Int, d: Int) => {
            n should be (1)
            d should be (2)
          }
          case _ => fail("Unable to match")
        }
      }
    }
  }
}
