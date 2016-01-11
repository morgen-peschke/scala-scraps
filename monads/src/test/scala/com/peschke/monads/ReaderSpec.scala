package com.peschke.monads

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ Matchers, WordSpec }

class ReaderSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {
  "run" should {
    "execute the wrapped function with the passed context" in {
      val addTen = (x: Int) => x + 10
      forAll((n: Int) => {

      })
    }
  }

  "flatMap" should {
    "return a Reader that is equivalent to modifying the result of the current reader" in (pending)
  }

  "map" should {
    "be equivalent to chaining functions" in (pending)
  }

  "foreach" should {
    "execute some operation based on the result of the reader" in (pending)
  }

  "Reader" should {
    "map in a for-comprehension" in (pending)

    "flatMap in a for-comprehension" in (pending)

    "foreach in a for-comprehension" in (pending)
  }
}
