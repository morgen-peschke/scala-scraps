package com.peschke.math.combinations

import scala.util.{Try, Success, Failure}

object PossibleDartsForScore {
  import CombinationGenerator._

  val possibleDarts = (0, 1) :: (25, 1) :: (25, 2) :: (for {
    wedge <- 1 to 20
    multiplier <- 1 to 3
  } yield (wedge, multiplier)).toList

  def possibleDartsForScore(score: Int) =
    possibleDarts
      .multiCombinations(3)
      .filter(_
        .map({case (wedge, m) => wedge * m})
        .foldLeft(0)(_ + _) == score)

  def formatDart(d: (Int, Int)): String = d match {
    case (0, _) =>  " - "
    case (w, 1) => f"$w%2d "
    case (w, 2) => f"$w%2dT"
    case (w, 3) => f"$w%2dD"
  }

  def scoreDart(d: (Int, Int)): Int = d match {
    case (wedge, m) => wedge * m
  }

  def progressBar(percent: Double,
                  length: Int = 100,
                  bar: Char = '-',
                  partial: Char = '.',
                  overflow: Char = '+',
                  space: Char = ' '): String = {
    if (percent > 100.0) {
      val b = List.fill(length - 1)(bar)
      s"${b}${overflow}"
    }
    else {
      val filledLength = percent * length
      val barLength = filledLength.floor.toInt
      val partialLength = if (filledLength.ceil == filledLength.floor) 0 else 1
      val p = List.fill(partialLength)(partial).mkString
      val b = List.fill(barLength)(bar).mkString
      val s = List.fill(length - barLength - partialLength)(space).mkString
      s"${b}${p}${s}"
    }
  }

  case class Bucket(start: Int, end: Int, count: Int) {
    def format(maxCount: Double): String = {
      val bucket =
        if (start == end) f"    ${start}%3d   "
        else f"${start}%-3d to ${end}%3d"
      val percent = count / maxCount
      val bar = progressBar(percent, 100)
      val flags = if (count == maxCount) "M" else ""
      s" ${bucket} |$bar  | ${count} $flags"
    }
  }
  object Bucket {
    def header(bucketSize: Int): String = {
      val title = "Histogram of dart combinations for scores, bucket size " + bucketSize
      f"""|.-----------.------------------------------------------------------------------------------------------------------.--------.
          ||  Buckets  | ${title}%-100s | Counts |
          |'-----------+------------------------------------------------------------------------------------------------------+--------'""".stripMargin
    }
  }

  def makeHistogram(bucketSize: Int): String = {
    def calculateBuckets: List[Bucket] = (1 to 180)
      .grouped(bucketSize)
      .map({ chunk =>
        val count = chunk.map(s => possibleDartsForScore(s).size).foldLeft(0)(_ + _)
        Bucket(chunk.min, chunk.max, count)
      }).toList

    if (bucketSize <= 0 || bucketSize > 180)
      "Bucket size must be between 1 and 180, inclusive"
    else {
      val buckets = calculateBuckets
      val maxCount = buckets.map(_.count).max.toDouble
      val lines = buckets.map(_.format(maxCount))
      val title = Bucket.header(bucketSize)
      (title :: lines).mkString("\n")
    }
  }

  def makeDartCombos(score: Int, numDarts: Int): String = {
    val maxScore = 60 * numDarts
    if (score < 0) "Score cannot be negative."
    else if (numDarts <= 0) "No darts to score with."
    else if (score > maxScore)
      s"A score of $score cannot be reached by $numDarts darts (max score is $maxScore)."
    else {
      val dartCombos = possibleDarts
        .multiCombinations(numDarts)
        .filter(_.map(scoreDart).foldLeft(0)(_ + _) == score)
        .map(_.toList.sorted)
        .map(_.map(formatDart).mkString(" "))

      val count = dartCombos.length
      val maxColumns = 180 / (numDarts * 4 + 5)
      val byColumns = dartCombos.grouped(((count / 30) + 1) min maxColumns)
        .map(_.mkString("     "))
        .toList

      val header = s"$count ways to achieve a score of $score, with $numDarts darts:"
      (header :: byColumns).mkString("\n")
    }
  }

  def toIntOrFail(input: String)(f: Int => String): String = Try(input.toInt) match {
    case Failure(ex) => s"${input} is not a number: ${ex.getMessage}"
    case Success(intValue) => f(intValue)
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println(makeHistogram(1))
    }
    else args.map(_.toLowerCase).foreach { arg =>
      if (arg.startsWith("h")) println(toIntOrFail(arg.tail)(makeHistogram))
      else if (arg.startsWith("s")) {
        if (arg.exists(_ == '@')) {
          val List(scoreArg, dartArg) = arg.tail.split("@").toList
          println(toIntOrFail(scoreArg) { score =>
            toIntOrFail(dartArg) { dartCount =>
              makeDartCombos(score, dartCount)
            }
          })
        }
        else println(toIntOrFail(arg.tail) {
          score => makeDartCombos(score, 3)
        })
      }
      else println(s"${arg} does not start with 's' or 'h'.")
    }
  }
}
