package com.peschke.codeReview

import org.scalatest.WordSpec
import org.scalatest.Matchers

class DivideListIntoNPartsSpec extends WordSpec with Matchers {
  import DivideListIntoNParts._

  val basicList = List(1, 2, 3, 1, 4, 5, 6, 1, 7)
  val basicSpans = List(List(1), List(2, 3), List(1), List(4, 5, 6), List(1), List(7))
  val doubleOnesList = List(1, 1, 2)
  val doubleOnesSpans = List(List(1), List(1), List(2))

  "Original.repeatedSpan" should {
    "split when the predicate returns true" in {
      Original.repeatedSpan(basicList, (a:Int) => a != 1) shouldBe basicSpans
    }

    "split multiple matches into separate elements" in {
      Original.repeatedSpan(doubleOnesList, (a: Int) => a != 1) shouldBe doubleOnesSpans
    }
  }

  "Curried.repeatedSpan" should {
    "split when the predicate returns true" in {
      Curried.repeatedSpan(basicList)(_ != 1) shouldBe basicSpans
    }

    "split multiple matches into separate elements" in {
      Curried.repeatedSpan(doubleOnesList)(_ != 1) shouldBe doubleOnesSpans
    }
  }

  "PatternMatching.repeatedSpan" should {
    "split when the predicate returns true" in {
      PatternMatching.repeatedSpan(basicList)(_ != 1) shouldBe basicSpans
    }

    "split multiple matches into separate elements" in {
      PatternMatching.repeatedSpan(doubleOnesList)(_ != 1) shouldBe doubleOnesSpans
    }
  }

  "TailRecursive.repeatedSpan" should {
    "split when the predicate returns true" in {
      TailRecursive.repeatedSpan(basicList)(_ != 1) shouldBe basicSpans
    }

    "split multiple matches into separate elements" in {
      TailRecursive.repeatedSpan(doubleOnesList)(_ != 1) shouldBe doubleOnesSpans
    }
  }

  "UsingVector.repeatedSpan" should {
    "split when the predicate returns true" in {
      UsingVector.repeatedSpan(basicList)(_ != 1) shouldBe basicSpans
    }

    "split multiple matches into separate elements" in {
      UsingVector.repeatedSpan(doubleOnesList)(_ != 1) shouldBe doubleOnesSpans
    }
  }
}
