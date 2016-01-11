package com.peschke.math.l_system

sealed trait Algae extends Variable[Algae]

object Algae extends LSystem[Algae] {

  case object A extends Algae {
    def rules: Iterator[Algae] = Iterator(A,B)
  }

  case object B extends Algae {
    def rules: Iterator[Algae] = Iterator(A)
  }

  def axiom: Iterator[Algae] = Iterator(A)
}
