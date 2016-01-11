package com.peschke.math.combinations

import org.scalatest.WordSpec
import org.scalatest.Matchers

class CombinationGeneratorSpec extends WordSpec with Matchers {
  import CombinationGenerator._

  "CombinationGenerator" should afterWord("return") {
    "the empty list for the empty list" in {
      Nil.multiCombinations(1) shouldBe Nil
      List[Int]().multiCombinations(1) shouldBe List[List[Int]]()
    }

    "a list of all combinations of requested length from the elements of a list" in {
      List(1,2).multiCombinations(0) shouldBe List(List())
      List(1,2).multiCombinations(1) shouldBe List(
        List(2),
        List(1))
      List(1,2).multiCombinations(2) shouldBe List(
        List(2,2),
        List(2,1),
        List(1,1))
      List(1,2).multiCombinations(3) shouldBe List(
        List(2,2,2),
        List(2,2,1),
        List(2,1,1),
        List(1,1,1))

      List(1,2,3).multiCombinations(0) shouldBe List(List())
      List(1,2,3).multiCombinations(1) shouldBe List(
        List(3),
        List(2),
        List(1))
      List(1,2,3).multiCombinations(2) shouldBe List(
        List(3,3),
        List(3,2),
        List(2,2),
        List(3,1),
        List(2,1),
        List(1,1))
      List(1,2,3).multiCombinations(3) shouldBe List(
        List(3,3,3),
        List(3,3,2),
        List(3,2,2),
        List(2,2,2),
        List(3,3,1),
        List(3,2,1),
        List(2,2,1),
        List(3,1,1),
        List(2,1,1),
        List(1,1,1))

      List(0,1,2,3).multiCombinations(3) shouldBe List(
        List(3,3,3),
        List(3,3,2),
        List(3,2,2),
        List(2,2,2),
        List(3,3,1),
        List(3,2,1),
        List(2,2,1),
        List(3,1,1),
        List(2,1,1),
        List(1,1,1),
        List(3,3,0),
        List(3,2,0),
        List(2,2,0),
        List(3,1,0),
        List(2,1,0),
        List(1,1,0),
        List(3,0,0),
        List(2,0,0),
        List(1,0,0),
        List(0,0,0))
    }
  }
}
