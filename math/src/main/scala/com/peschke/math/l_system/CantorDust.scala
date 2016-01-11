package com.peschke.math.l_system

sealed trait CantorDust extends Variable[CantorDust]

object CantorDust extends LSystem[CantorDust] {

  case object FILL extends CantorDust {
    def rules: Iterator[CantorDust] = Iterator(FILL,SPACE,FILL)
  }

  case object SPACE extends CantorDust {
    def rules: Iterator[CantorDust] = Iterator(SPACE,SPACE,SPACE)
  }

  def axiom: Iterator[CantorDust] = Iterator(FILL)

  def draw(width: Int, maxGenerations: Option[Int] = None): String = {
    def widths = Iterator.iterate(LSystem.floorLog3(Math.abs(width)))(_ / 3).takeWhile(_ >= 1)
    def generations = Iterator.iterate(0)(_ + 1).map(CantorDust.generation _)
    def numGenerations = maxGenerations.getOrElse(width)

    val lines = (widths zip generations).take(Math.abs(numGenerations)).map({
      case (width, current) => current.map({
        case FILL => "-" * width
        case SPACE => " " * width
      }).mkString
    }).toVector

    (if (numGenerations < 0) lines.reverse
     else lines).mkString("\n")
  }
}

object CantorDustFractal {
  def main (args: Array[String]): Unit =
    if (args.isEmpty) println(CantorDust.draw(80))
    else args.map(_.split(",")).map(_.map(_.toInt)).map({
      case Array(w)    => CantorDust.draw(w, None)
      case Array(w, 0) => CantorDust.draw(w, None)
      case Array(w, g) => CantorDust.draw(w, Some(g))
    }).foreach(println)
}
