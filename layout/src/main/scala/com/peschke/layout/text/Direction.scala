package com.peschke.layout.text

sealed abstract class Direction(prevDir: => Direction, nextDir: => Direction) {
  def prev: Direction = prevDir
  def next: Direction = nextDir
}

object Direction {
  def apply(input: String): Direction = input.trim.toLowerCase match {
    case "n"  | "north"      => North
    case "ne" | "northeast"  => NorthEast
    case "e"  | "east"       => East
    case "se" | "southeast"  => SouthEast
    case "s"  | "south"      => South
    case "sw" | "southwest"  => SouthWest
    case "w"  | "west"       => West
    case "nw" | "northwest"  => NorthWest
  }

  case object North     extends Direction(NorthWest, NorthEast)
  case object NorthEast extends Direction(North,     East     )
  case object East      extends Direction(NorthEast, SouthEast)
  case object SouthEast extends Direction(East,      South    )
  case object South     extends Direction(SouthEast, SouthWest)
  case object SouthWest extends Direction(South,     West     )
  case object West      extends Direction(SouthWest, NorthWest)
  case object NorthWest extends Direction(West,      North    )
}
