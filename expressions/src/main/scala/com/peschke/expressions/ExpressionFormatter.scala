package com.peschke.expressions

import com.peschke.layout.text.{Element, EmptyElement, Direction}
import com.peschke.layout.text.ElementComposer._

class ExpressionFormatter(shownDecimals: Int = 1) {

  def formatDouble(d: Double): String = {
    d.toString.split('.').toList match {
      case i :: Nil => i
      case i :: decimalParts =>
        val dPart = decimalParts
          .mkString
          .toList
          .take(shownDecimals)
          .reverse
          .dropWhile(_ == '0')
          .reverse
          .mkString

        if (dPart.isEmpty) i else s"$i.$dPart"

      case Nil => "0"
    }
  }

  private implicit val composerSettings: Settings = Settings()
  private val lParen = Element('(')
  private val rParen = Element(')')
  private val space = Element(' ')

  private def format(e: Expression, enclPrec: Int): Element = e match {
    case Var(name) => Element(name)
    case Number(num) => Element(formatDouble(num))
    case UnOp(op, arg) => Element(op) ++ format(arg, Expression.unaryPrecedence)

    case BinOp(op, left, right) =>
      val opPrec = Expression.binaryPrecedence(op)
      val l = format(left, opPrec)
      val r = format(right, opPrec + 1)
      val oper = Element(" " + op + " ")
      val exp = l ++ oper ++ r
      if (enclPrec <= opPrec) exp
      else lParen ++ exp ++ rParen

    case AbsValue(arg) =>
      val inner = {
        val formatted = format(arg, Expression.functionPrecedence)
        arg match {
          case _: AbsValue => space / formatted / space
          case _ if (enclPrec == Expression.functionPrecedence) => space ++ formatted ++ space
          case _ => formatted
        }
      }
      val side = Element('|', 1, inner.height)
      side ++ inner ++ side

    case Fraction(topArg, bottomArg) =>
      def formatWithPadding(e: Expression): Element = {
        val inner = format(e, Expression.functionPrecedence)
        e match {
          case _: Fraction => space ++ inner ++ space
          case _ => inner
        }
      }
      val top = formatWithPadding(topArg)
      val bottom = formatWithPadding(bottomArg)
      val line = Element('-', top.width max bottom.width, 1)
      val frac = top / line / bottom

      if (enclPrec == Expression.unaryPrecedence) space ++ frac
      else frac

    case Root(degree, radicand) =>
      val outside = {
        val degreeFormatted = degree match {
          case Number(2.0) => EmptyElement
          case _ => format(degree, Expression.functionPrecedence) ++ space
        }
        val hook = Element('\\')
        degreeFormatted.above(hook)(composerSettings.copy(anchor = Some(Direction.East)))
      }
      val inside = space ++ format(radicand, Expression.functionPrecedence) ++ space

      val height = inside.height max outside.height

      val top = Element('+') ++ Element('-', inside.width, 1)
      val side = Element('|', 1, height)

      (outside padNorth (height + 1)) ++ (top above (side ++ inside))
  }

  def format(e: Expression): Element = format(e, 0)
}
