package com.peschke.codeReview

import org.scalatest.{Matchers, WordSpec}

class GreedySubsetSpec extends WordSpec with Matchers {

  "GreedySubset" should {
    "return the selected values in selection order" in {
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 4) shouldBe Seq(1,2,3,4)
      GreedySelectEvenOdd.selectSubset(Seq(4,3,2,1), 4) shouldBe Seq(1,2,3,4)
    }

    "selectNext based on the computed state" in {
      GreedySelectEvenOdd.selectSubset(Seq(1,6,3,4), 4) shouldBe Seq(1,4,3,6)
      GreedySelectEvenOdd.selectSubset(Seq(7,2,5,4), 4) shouldBe Seq(5,2,7,4)
    }

    "not skip the initial state" in {
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 4) shouldBe Seq(1,2,3,4)
      GreedySelectEvenOdd.selectSubset(Seq(2,3,4,5), 4) shouldBe Seq(3,2,5,4)
    }

    "selectNext based on the already selected values" in {
      GreedySelectEvenOdd.selectSubset(Seq(1,2,31,4), 4) shouldBe Seq(1,2,31)
      GreedySelectEvenOdd.selectSubset(Seq(101,2,301,4), 4) shouldBe Seq(101)
    }

    "terminate after k values" in {
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 3) shouldBe Seq(1,2,3)
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 2) shouldBe Seq(1,2)
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 1) shouldBe Seq(1)
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 0) shouldBe Seq()
    }

    "terminate early if there are fewer than k values" in {
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 5) shouldBe Seq(1,2,3,4)
      GreedySelectEvenOdd.selectSubset(Seq(1,2,3,4), 6) shouldBe Seq(1,2,3,4)
    }

    "terminate early if nothing can be chosen" in {
      GreedySelectEvenOdd.selectSubset(Seq(1,3,5,7), 4) shouldBe Seq(1)
    }
  }

  "GreedySubset.selector" should {
    "return the selected values in selection order" in {
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 4) shouldBe Seq(1,2,3,4)
      GreedySelectEvenOdd.selector.selectSubset(Seq(4,3,2,1), 4) shouldBe Seq(1,2,3,4)
    }

    "selectNext based on the computed state" in {
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,6,3,4), 4) shouldBe Seq(1,4,3,6)
      GreedySelectEvenOdd.selector.selectSubset(Seq(7,2,5,4), 4) shouldBe Seq(5,2,7,4)
    }

    "not skip the initial state" in {
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 4) shouldBe Seq(1,2,3,4)
      GreedySelectEvenOdd.selector.selectSubset(Seq(2,3,4,5), 4) shouldBe Seq(3,2,5,4)
    }

    "selectNext based on the already selected values" in {
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,31,4), 4) shouldBe Seq(1,2,31)
      GreedySelectEvenOdd.selector.selectSubset(Seq(101,2,301,4), 4) shouldBe Seq(101)
    }

    "terminate after k values" in {
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 3) shouldBe Seq(1,2,3)
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 2) shouldBe Seq(1,2)
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 1) shouldBe Seq(1)
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 0) shouldBe Seq()
    }

    "terminate early if there are fewer than k values" in {
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 5) shouldBe Seq(1,2,3,4)
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,2,3,4), 6) shouldBe Seq(1,2,3,4)
    }

    "terminate early if nothing can be chosen" in {
      GreedySelectEvenOdd.selector.selectSubset(Seq(1,3,5,7), 4) shouldBe Seq(1)
    }
  }

  object GreedySelectEvenOdd extends GreedySubset {
    type O = Int
    type S = Boolean

    val maxSize = 20

    def initialState(candidates: Seq[O]): S = false // Start with odd values

    def selectNext(candidates: Seq[O], alreadySelected: Seq[O], state: S): Option[O] =
      if (alreadySelected.sum > 20) None
      else candidates.filter(if (state) evens else odds).reduceOption(_ min _)

    def computeState(previousState:S, lastSelected:O): S = !previousState

    val odds = (x: Int) => x % 2 == 1
    val evens = (x: Int) => !odds(x)

    val selector = GreedySelector(initialState _, selectNext _, computeState _)
  }
}
