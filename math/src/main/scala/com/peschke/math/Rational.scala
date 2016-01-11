package com.peschke.math

import scala.annotation.tailrec
import scala.language.implicitConversions

class Rational(numerator: Int, denominator: Int) extends Ordered[Rational] {
  require(denominator != 0, "denominator cannot be 0")

  private val g = gcd(numerator.abs, denominator.abs)
  private val neg = (numerator < 0) ^ (denominator < 0)
  private val n = numerator.abs / g * (if (neg) -1 else 1)
  private val d = denominator.abs / g

  def * (that: Rational) = new Rational(
    this.n * that.n,
    this.d * that.d)

  def / (that: Rational) = new Rational(
    this.n * that.d,
    this.d * that.n)

  def + (that: Rational) = new Rational(
    this.n * that.d + that.n * this.d,
    this.d * that.d)

  def - (that: Rational) = new Rational(
    this.n * that.d - that.n * this.d,
    this.d * that.d)

  //def < (that: Rational) = (this.n * that.d) < (that.n * this.d)

  def compare(that: Rational): Int = (this.n * that.d) - (that.n * this.d)

  override def toString: String = if(d == 1) s"$n" else s"${n}/${d}"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Rational]
  override def equals(other: Any): Boolean =
    other match {
      case that: Rational => (that canEqual this) &&
        this.n == that.n &&
        this.d == that.d
      case _ => false
    }
  override def hashCode: Int =
    41 * (
      41 + n.hashCode
    ) + d.hashCode

  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}

object Rational {
  implicit def apply(q: Double): Rational = {
    def isFloatGoodEnough(guess: Double): Boolean = Math.abs(guess - q) / q < 0.0001
    def isGoodEnough(guess: (Int,Int)): Boolean = guess match {
      case (n, d) if d != 0 => isFloatGoodEnough(n.toDouble / d.toDouble)
      case _ => false
    }

    def calculateMediant(l: (Int,Int), h: (Int,Int)): (Int, Int) = (l, h) match {
      case ((a, b), (c, d)) => (a + c, b + d)
    }
    def mediantAsDouble(m: (Int,Int)): Double = m match {
      case (n, d) => n.toDouble / d.toDouble
    }

    @tailrec
    def sternBrocot(l: (Int,Int), h: (Int,Int)): Rational = {
      val m = calculateMediant(l, h)
      val fm = mediantAsDouble(m)

      if (isGoodEnough(l)) Rational(l)
      else if (isGoodEnough(h)) Rational(h)
      else if (fm < q) sternBrocot(m, h)
      else if (fm > q) sternBrocot(l, m)
      else Rational(m)
    }
    sternBrocot((0,1), (1,0))
  }
  implicit def apply(n: Int): Rational = new Rational(n, 1)
  def apply(v: (Int,Int)): Rational = v match {
    case (n:Int, d:Int) => new Rational(n,d)
  }
  def apply(n: Int, d: Int): Rational = new Rational(n,d)
  def unapply(r: Rational): Option[(Int,Int)] = Some((r.n, r.d))
}
