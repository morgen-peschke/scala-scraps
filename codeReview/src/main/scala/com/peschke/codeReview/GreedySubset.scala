package com.peschke.codeReview

import scala.annotation.tailrec

trait GreedySubset {
  type O /* Individual objects */
  type S /* Intermediate state */

  def selectSubset(candidates: Seq[O], k: Int): Seq[O] = {
    @tailrec
    def loop(candidates: Seq[O], acc: Seq[O], count: Int, state: S): Seq[O] =
      if (candidates.isEmpty || count <= 0) acc
      else selectNext(candidates, acc, state) match {
        case Some(next) =>
          loop(candidates.filterNot(_ == next),
            acc :+ next,
            count - 1,
            computeState(state, next))
        case None => acc
      }
    loop(candidates, Seq[O](), k, initialState(candidates))
  }

  /* Implemented by library users */
  def initialState(candidates: Seq[O]): S

  def selectNext(candidates: Seq[O], alreadySelected: Seq[O], state: S): Option[O]

  def computeState(previousState:S, lastSelected:O): S
}

case class GreedySelector[E,S](
  initialState: Seq[E] => S,
  selectNext: (Seq[E], Seq[E], S) => Option[E], // First argument is candidates
  computeState: (S, E) => S) {

  def selectSubset(candidates: Seq[E], k: Int): Seq[E] = {
    @tailrec
    def loop(candidates: Seq[E], acc: Seq[E], count: Int, state: S): Seq[E] =
      if (candidates.isEmpty || count <= 0) acc
      else selectNext(candidates, acc, state) match {
        case Some(next) =>
          loop(candidates.filterNot(_ == next),
            acc :+ next,
            count - 1,
            computeState(state, next))
        case None => acc
      }
    loop(candidates, Seq[E](), k, initialState(candidates))
  }
}
