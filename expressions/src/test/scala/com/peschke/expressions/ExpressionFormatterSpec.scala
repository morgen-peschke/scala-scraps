package com.peschke.expressions

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import com.peschke.layout.text.Element
import com.peschke.layout.text.ElementComposer._
import ExpressionFormatterSpecImplicits._
import Expression._

class ExpressionFormatterSpec extends WordSpec with Matchers with ElementMatchers {
  val format = afterWord("format")

  "formatDouble" should {
    "drop the trailing .0 on integer values" in {
      val formatter = new ExpressionFormatter()
      formatter.formatDouble(1.0) should be ("1")
      formatter.formatDouble(2.0) should be ("2")
      formatter.formatDouble(3.0) should be ("3")
      formatter.formatDouble(4.0) should be ("4")
      formatter.formatDouble(5.0) should be ("5")
    }

    "truncate past the shownDecimals settings" in {
      val formatter = new ExpressionFormatter(3)

      formatter.formatDouble(1.1234) should be ("1.123")
      formatter.formatDouble(2.0123) should be ("2.012")
      formatter.formatDouble(3.0012) should be ("3.001")
    }

    "collapse trailing zeros" in {
      val formatter = new ExpressionFormatter(3)

      formatter.formatDouble(1.12034) should be ("1.12")
      formatter.formatDouble(2.10023) should be ("2.1")
      formatter.formatDouble(3.00012) should be ("3")
    }
  }

  "format" should afterWord("render"){
    val formatter = new ExpressionFormatter(2)

    "Vars as the variable name" in {
      formatter.format(Var("x")).render should be ("x")
      formatter.format(Var("longVariableName")).render should be ("longVariableName")
      formatter.format("cast from string").render should be ("cast from string")
    }

    "Numbers as the wrapped value" in {
      formatter.format(Number(1)).render should be ("1")
      formatter.format(Number(3.0012)).render should be ("3")
      formatter.format(Number(3.012)).render should be ("3.01")
      formatter.format(3.012).render should be ("3.01")
    }

    "UnOps as the symbol followed by the rendered arg" in {
      formatter.format(Op("-", "x")).render should be ("-x")
      formatter.format(Op("-", 1.2)).render should be ("-1.2")
      formatter.format(Op("-", add(2, 3))).render should be ("-(2 + 3)")
    }

    "BinOps as an infix expression" in {
      formatter.format(add("a", 1.2)).render should be ("a + 1.2")
    }

    "Fractions inside a UnOp with a leading space" in {
      formatter.format(Op("-", frac("a", "b"))) should renderTo(
        """|  a
           |- -
           |  b""")
    }

    "Fractions arranged vertically" in {
      formatter.format(frac("a", "b")) should renderTo(
        """|a
           |-
           |b""")
      formatter.format(frac("num", "denominator")).withBorder should renderTo(
        """|#############
           |#    num    #
           |#-----------#
           |#denominator#
           |#############""")
    }

    "nested Fractions with decreasing line size at each level of nesting" in {
      formatter.format(frac(frac("a", "b"), frac("c", "d"))).withBorder should renderTo(
        """|#####
           |# a #
           |# - #
           |# b #
           |#---#
           |# c #
           |# - #
           |# d #
           |#####""")
      formatter.format(
        frac(frac(frac("a", "b"), frac("c", "d")), "e")).withBorder should renderTo(
        """|#######
           |#  a  #
           |#  -  #
           |#  b  #
           |# --- #
           |#  c  #
           |#  -  #
           |#  d  #
           |#-----#
           |#  e  #
           |#######""")
    }

    "constants inside AbsValue as absolute value bars without spaces" in {
      formatter.format(abs("x")).render should be ("|x|")
    }

    "AbsValue with the argument wrapped in a large absolute value symbol" in {
      formatter.format(add(3, abs(frac(3, 2) + 1.2))) should renderTo(
        """|    |3      |
           |3 + |- + 1.2|
           |    |2      |""")
      formatter.format(add(3, abs(abs(abs(frac(3, 2)))))) should renderTo(
        """|    |       |
           |    ||     ||
           |    ||| 3 |||
           |3 + ||| - |||
           |    ||| 2 |||
           |    ||     ||
           |    |       |""")
      formatter.format(frac(abs(frac(1, 2)), 3)).withBorder should renderTo(
        """|#######
           |#| 1 |#
           |#| - |#
           |#| 2 |#
           |#-----#
           |#  3  #
           |#######""")
    }

    "Root of degree 2 as the argument wrapped in a large square root symbol" in {
      formatter.format(sqrt(frac(3, 2) + 5)).withBorder should renderTo(
        """|###########
           |# +-------#
           |# | 3     #
           |# | - + 5 #
           |#\| 2     #
           |###########""")
    }

    "Root as the right argument wrapped in a large N root symbol, where N is the degree" in {
      formatter.format(root(4, frac(3, 2) + 5)).withBorder should renderTo(
        """|############
           |#  +-------#
           |#  | 3     #
           |#4 | - + 5 #
           |# \| 2     #
           |############""")
      formatter.format(root(frac(1.5, "x"), add("x", 1)))
        .withBorder should renderTo(
        """|##############
           |#    +-------#
           |#1.5 |       #
           |#--- | x + 1 #
           |# x  |       #
           |#   \|       #
           |##############""")
    }

    "complex expressions composed from the formatted subexpressions" in {
      val e1 = frac(1, 2) * add("x", 1)
      val e2 = frac("x", 2) + frac(1.5, "x")

      formatter.format(e1).withBorder should renderTo(
        """|#############
           |#1          #
           |#- * (x + 1)#
           |#2          #
           |#############""")
      formatter.format(e2).withBorder should renderTo(
        """|#########
           |#x   1.5#
           |#- + ---#
           |#2    x #
           |#########""")
      formatter.format(frac(e1, e2)).withBorder should renderTo(
        """|#############
           |#1          #
           |#- * (x + 1)#
           |#2          #
           |#-----------#
           |#  x   1.5  #
           |#  - + ---  #
           |#  2    x   #
           |#############""")
    }
  }
}

object ExpressionFormatterSpecImplicits {
  implicit class RichElement(val e: Element) extends AnyVal {
    def withBorder: Element = {
      val horizontal = Element('#', e.width + 2, 1)
      val vertical = Element('#', 1, e.height)
      horizontal / (vertical ++ e ++ vertical) / horizontal
    }
  }
}

trait ElementMatchers {
  def renderTo(block: String): Matcher[Element] = Matcher { (element: Element) =>
    val rendered = element.render
    val expected = block.stripMargin
    MatchResult(
      rendered == expected,
      s"""|Expected value:
          |
          |${expected}
          |
          |was not equal to:
          |
          |${rendered}
          |
          |""".stripMargin,
      s"""|Expected value:
          |
          |${expected}
          |
          |was equal to:
          |
          |${rendered}
          |
          |""".stripMargin)
  }
}
