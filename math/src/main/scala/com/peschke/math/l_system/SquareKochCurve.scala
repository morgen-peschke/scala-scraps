package com.peschke.math.l_system

import scala.annotation.tailrec

sealed trait SquareKochCurve extends Variable[SquareKochCurve]

object SquareKochCurve extends LSystem[SquareKochCurve] {
  case object F extends SquareKochCurve {
    def rules: Iterator[SquareKochCurve] = Iterator(F,L,F,R,F,R,F,L,F)
  }

  case object L extends SquareKochCurve {
    def rules: Iterator[SquareKochCurve] = Iterator.empty
  }

  case object R extends SquareKochCurve {
    def rules: Iterator[SquareKochCurve] = Iterator()
  }

  def axiom: Iterator[SquareKochCurve] = Iterator(F)

  def draw(generationNumber: Int, segmentLength: Option[Int] = None): String = {
    def genPoints(points: Map[(Int,Int),Char],
                  location: (Int,Int,Direction),
                  instructions: Iterator[SquareKochCurve]): Map[(Int,Int),Char] =
      (location, instructions) match {
        case (_, instructions) if instructions.isEmpty => points
        case ((row, col, heading), instructions) =>
          val instruction = instructions.next
          val charToDrop = composeOver(glyph(heading, instruction), points.get(row -> col))
          val newHeading = heading after instruction
          val (newRow, newCol) = nextPoint(row, col, newHeading)
          genPoints(points + ((row, col) -> charToDrop), (newRow, newCol, newHeading), instructions)
      }
    def nextPoint(row: Int, col: Int, heading: Direction): (Int,Int) = heading match {
      case UP    => (row + 1, col)
      case DOWN  => (row - 1, col)
      case LEFT  => (row, col - 1)
      case RIGHT => (row, col + 1)
    }
    def glyph(heading: Direction, instruction: SquareKochCurve): Char =
      (heading, instruction) match {
        case (UP,    F) | (DOWN,  F) => '|'
        case (LEFT,  F) | (RIGHT, F) => '─'
        case (UP,    R) | (LEFT,  L) => '┌'
        case (UP,    L) | (RIGHT, R) => '┐'
        case (DOWN,  R) | (RIGHT, L) => '┘'
        case (DOWN,  L) | (LEFT,  R) => '└'
      }
    def composeOver(newGlyph: Char, oldGlyph: Option[Char]): Char = oldGlyph match {
      case None    => newGlyph
      case Some(_) => '┼'
    }
    def instructions = SquareKochCurve.generation(generationNumber).flatMap({
      case F => Iterator.fill(segmentLength.getOrElse(defaultSegmentLength(generationNumber)))(F)
      case i => Iterator(i)
    })
    val points = genPoints(Map().withDefaultValue(' '),
                           (0,0,RIGHT),
                           instructions)
    val (maxRow, maxCol) = points.keys.foldLeft(0 -> 0)({
      case ((r1, c1), (r2, c2)) => (r1 max r2, c1 max c2)
    })
    Vector.tabulate[Char](maxRow + 1, maxCol + 1)((r,c) => points(r -> c))
      .map(_.mkString)
      .reverse
      .mkString("\n") //+ s" ${maxRow + 1} x ${maxCol + 1}"
  }

  private def defaultSegmentLength(generationNumber: Int): Int = generationNumber match {
    case 0 => 80
    case 1 => 26
    case 2 => 8
    case 3 => 2
    case _ => 1
  }

  private sealed trait Direction {
    def after(instruction: SquareKochCurve): Direction = (this, instruction) match {
      case (_,     F) => this
      case (UP,    L) => LEFT
      case (UP,    R) => RIGHT
      case (DOWN,  L) => RIGHT
      case (DOWN,  R) => LEFT
      case (LEFT,  L) => DOWN
      case (LEFT,  R) => UP
      case (RIGHT, L) => UP
      case (RIGHT, R) => DOWN
    }
  }
  private case object UP    extends Direction
  private case object DOWN  extends Direction
  private case object LEFT  extends Direction
  private case object RIGHT extends Direction
}

object SquareKochCurveFractal {
  def main (args: Array[String]): Unit = args match {
    case a if a.isEmpty => println(SquareKochCurve.draw(3, Some(2)))
    case Array("big") =>
      Seq(
        SquareKochCurve.draw(4, Some(  1)),
        SquareKochCurve.draw(3, Some(  5)),
        SquareKochCurve.draw(2, Some( 17)),
        SquareKochCurve.draw(1, Some( 53)),
        SquareKochCurve.draw(0, Some(161))).foreach(println)
    case Array("small") =>
      Seq(
        SquareKochCurve.draw(3, Some( 1)),
        SquareKochCurve.draw(2, Some( 5)),
        SquareKochCurve.draw(1, Some(17)),
        SquareKochCurve.draw(0, Some(53))).foreach(println)
    case _ =>
      args.map(_.split(",")).map(_.map(_.toInt)).map({
        case Array(g)    => SquareKochCurve.draw(g, None)
        case Array(g, l) => SquareKochCurve.draw(g, Some(l))
      }).foreach(println)
  }
}
