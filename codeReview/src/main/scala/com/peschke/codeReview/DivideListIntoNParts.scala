package com.peschke.codeReview

import scala.annotation.tailrec

object DivideListIntoNParts {
  object Original {
    def repeatedSpan[T](iterable: Iterable[T], pred: T => Boolean): List[Iterable[T]] = {
      if (iterable.isEmpty) Nil
      else {
        val (matches, rest) = iterable.span(pred)
        if (matches.isEmpty) rest.take(1) :: repeatedSpan(rest.tail, pred)
        else matches :: repeatedSpan(rest, pred)
      }
    }
  }

  object Curried {
    def repeatedSpan[T](col: Iterable[T])(pred: T => Boolean): List[Iterable[T]] = {
      if (col.isEmpty) Nil
      else {
        val (a, b) = col.span(pred)
        if (a.isEmpty) b.take(1) :: repeatedSpan(b.tail)(pred)
        else a :: repeatedSpan(b)(pred)
      }
    }
  }

  object PatternMatching {
    def repeatedSpan[T](iterable: Iterable[T])(pred: T => Boolean): List[Iterable[T]] =
      iterable.span(pred) match {
        case (Nil, doesNotMatch :: unTested) =>
          List(doesNotMatch) :: repeatedSpan(unTested)(pred)
        case (matchingPrefix, doesNotMatch :: unTested) =>
          matchingPrefix :: List(doesNotMatch) :: repeatedSpan(unTested)(pred)
        case (matchingPrefix, Nil) => List(matchingPrefix)
      }
  }

  object TailRecursive {
    def repeatedSpan[T](iterable: Iterable[T])(pred: T => Boolean): List[Iterable[T]] = {
      @tailrec
      def loop(accum: List[Iterable[T]], rest: Iterable[T]): List[Iterable[T]] =
        rest.span(pred) match {
          case (Nil, doesNotMatch :: unTested) =>
            loop(List(doesNotMatch) :: accum, unTested)
          case (matchingPrefix, doesNotMatch :: unTested) =>
            loop(List(doesNotMatch) :: matchingPrefix :: accum, unTested)
          case (matchingPrefix, Nil) => List(matchingPrefix)
            matchingPrefix :: accum
        }
      loop(Nil, iterable).reverse
    }
  }

  object UsingVector {
    def repeatedSpan[T](iterable: Iterable[T])(pred: T => Boolean): Iterable[Iterable[T]] = {
      @tailrec
      def loop(accum: Vector[Iterable[T]], rest: Iterable[T]): Iterable[Iterable[T]] =
        rest.span(pred) match {
          case (Nil, doesNotMatch :: unTested) => loop(accum :+ List(doesNotMatch), unTested)
          case (matches, doesNotMatch :: unTested) => loop(accum :+ matches :+ List(doesNotMatch), unTested)
          case (matches, Nil) => accum :+ matches
        }
      loop(Vector(), iterable)
    }
  }
}
