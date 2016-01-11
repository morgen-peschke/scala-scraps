package com.peschke.math.l_system

import org.scalatest.{Matchers,WordSpec}
import Algae.{A,B}

class AlgaeSpec extends WordSpec with Matchers {
  "Algae" should afterWord("produce at"){
    "n = 0 : A" in {
      Algae.generation(0).toSeq shouldBe Seq(A)
    }

    "n = 1 : AB" in {
      Algae.generation(1).toSeq shouldBe Seq(A,B)
    }

    "n = 2 : ABA" in {
      Algae.generation(2).toSeq shouldBe Seq(A,B,A)
    }

    "n = 3 : ABAAB" in {
      Algae.generation(3).toSeq shouldBe Seq(A,B,A,A,B)
    }

    "n = 4 : ABAABABA" in {
      Algae.generation(4).toSeq shouldBe Seq(A,B,A,A,B,A,B,A)
    }

    "n = 5 : ABAABABAABAAB" in {
      Algae.generation(5).toSeq shouldBe Seq(A,B,A,A,B,A,B,A,A,B,A,A,B)
    }
  }
}
