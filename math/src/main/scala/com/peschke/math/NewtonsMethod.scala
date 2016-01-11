package com.peschke.math

import scala.annotation.tailrec

object NewtonsMethod {
  def sqrt(x: Double) = {
    require(x >= 0, "Square Root is undefined for negative numbers")

    def isGoodEnough(guess: Double) = Math.abs(guess * guess - x) / x < 0.0001
    def improve(guess: Double) = (guess + x / guess) / 2

    @tailrec
    def iter(guess: Double): Double =
      if(isGoodEnough(guess)) guess
      else iter(improve(guess))

    iter(1.0)
  }
}
