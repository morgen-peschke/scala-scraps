package com.peschke.math.l_system

import scala.annotation.tailrec

trait Variable[T <: Variable[T]] {
  def rules: Iterator[T]
}

trait LSystem[T <: Variable[T]] {
  def axiom: Iterator[T]
  def generation(n: Int): Iterator[T] = {
    @tailrec
    def loop(current: Iterator[T], generation: Int): Iterator[T] =
      if (generation == n) current
      else loop(LSystem.applyRules(current), generation + 1)
    loop(axiom, 0)
  }
}

object LSystem {
  def applyRules[T <: Variable[T]](current: Iterator[T]): Iterator[T] =
    current.flatMap(variable => variable.rules match {
      case replacements if replacements.isEmpty => Iterator(variable) // Identity rule
      case replacements => replacements
    })

  def floorLog3(n: Int): Int =
    if (n <= 1) 1
    else Iterator.iterate(1)(_ * 3).takeWhile(_ <= n).reduceLeft((_,b) => b)
}
