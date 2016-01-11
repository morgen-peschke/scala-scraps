package com.peschke.expressions

import com.peschke.layout.text.Element
import com.peschke.layout.text.ElementComposer._
import scala.language.implicitConversions

sealed trait Expression
case class Var(name: String) extends Expression
case class Number(num: Double) extends Expression
case class UnOp(operator: String, arg: Expression) extends Expression
case class BinOp(operator: String, left: Expression, right: Expression) extends Expression

case class AbsValue(arg: Expression) extends Expression
case class Fraction(top: Expression, bottom: Expression) extends Expression
case class Root(degree: Expression, radicand: Expression) extends Expression

object Op {
  def apply(op: String, arg: Expression): Expression = UnOp(op, arg)
  def apply(op: String, l: Expression, r: Expression): Expression = BinOp(op, l, r)
}

object Expression {
  implicit val settings = Settings(fill = ' ')

  type DefaultFormatter = (Expression, Int) => Element

  private val opGroups = Seq(
    Set("|", "||"),
    Set("&", "&&"),
    Set("^"),
    Set("==", "!="),
    Set("<", "<=", ">", ">="),
    Set("+", "-"),
    Set("/", "*", "%"))

  // A mapping from operators to their precedence
  val binaryPrecedence = (for {
    (group, i) <- opGroups.zipWithIndex
    op <- group
  } yield op -> i).toMap

  val unaryPrecedence = opGroups.length
  val functionPrecedence = -1

  // Factories

  def add(l: Expression, r: Expression): BinOp = BinOp("+", l, r)
  def sub(l: Expression, r: Expression): BinOp = BinOp("-", l, r)
  def mul(l: Expression, r: Expression): BinOp = BinOp("*", l, r)
  def mod(l: Expression, r: Expression): BinOp = BinOp("%", l, r)
  def div(l: Expression, r: Expression): BinOp = BinOp("/", l, r)

  def abs(arg: Expression): AbsValue = AbsValue(arg)
  def frac(l: Expression, r: Expression): Fraction = Fraction(l, r)
  def sqrt(arg: Expression): Root = Root(Number(2), arg)
  def root(degree: Expression, arg: Expression): Root = Root(degree, arg)

  // Implicits

  implicit def doubleToNumber(d: Double): Expression = Number(d)
  implicit def stringToVar(s: String): Expression = Var(s)

  implicit class RichExpression(val e: Expression) extends AnyVal {
    def + (o: Expression): Expression = add(e, o)
    def - (o: Expression): Expression = sub(e, o)
    def * (o: Expression): Expression = mul(e, o)
    def / (o: Expression): Expression = div(e, o)
    def % (o: Expression): Expression = mod(e, o)
  }
}
