package com.peschke.math

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import NewtonsMethodSpec._

class NewtonsMethodSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {
  "NewtonsMethod.sqrt" should {
    "work for very large numbers" in {
      NewtonsMethod.sqrt(1e50).round(2) shouldBe Math.sqrt(1e50).round(2)
    }
    "be equivalent to Math.sqrt" in {
      forAll {
        (d: Double) => whenever(d >= 0) {
          NewtonsMethod.sqrt(d).round(3) shouldBe Math.sqrt(d).round(3)
        }
      }
    }
  }
}

object NewtonsMethodSpec {
  implicit class Roundable(val d: Double) extends AnyVal {
    def round(scale: Int): BigDecimal =
      BigDecimal(d).round(new java.math.MathContext(scale, java.math.RoundingMode.FLOOR))
  }
}
