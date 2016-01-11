package com.peschke.math.combinations

import scala.annotation.tailrec

object CombinationGenerator {
  implicit class CombinationGeneratingList[T](val list: List[T]) extends AnyVal {
    def multiCombinations(length: Int): List[List[T]] = {
      lazy val translate = list.toVector
      lazy val maxIndex = translate.length - 1
      def initialCombo = List.fill(length)(0)
      @tailrec
      def loop(acc: List[List[Int]]): List[List[Int]] =
        acc.head.dropWhile(_ >= maxIndex) match {
          case Nil => acc
          case x :: xs =>
            val n = x + 1
            @tailrec
            def preFill(acc: List[Int], l: Int): List[Int] =
              if (0 == l) acc
              else preFill(n :: acc, l - 1)
            val nextCombo = preFill(xs, length - xs.length)
            loop(nextCombo :: acc)
        }
      val indexes = if (list.isEmpty) List() else loop(List(initialCombo))
      indexes.map(_.map(translate))
    }
  }
}
