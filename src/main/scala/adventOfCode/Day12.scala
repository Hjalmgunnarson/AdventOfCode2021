package adventOfCode

import scala.io.Source

object Day12 extends App {
  val lines = Source.fromResource("day12Input.txt").getLines.toList
  val pattern = "([NESWLRF])([\\d]+)".r
  val moves = for {
    line <- lines
    pattern(h, d) = line
  } yield Move(h.toCharArray.head, d.toInt)

  val finalShip = moves.foldLeft(Ship(Position(0, 0), East))((ship, move) => ship.sail(move))
  // Part 1
  println(finalShip.position.getManhattanDistance)
}

case class Ship(position: Position, heading: Heading) {

  def sail(move: Move): Ship = {
    move.direction match {
      case 'N' => Ship(position.change(North, move.amount), heading)
      case 'E' => Ship(position.change(East, move.amount), heading)
      case 'S' => Ship(position.change(South, move.amount), heading)
      case 'W' => Ship(position.change(West, move.amount), heading)
      case 'F' => Ship(position.change(heading, move.amount), heading)
      case 'L' => Ship(position, heading.changeCourse(-move.amount))
      case 'R' => Ship(position, heading.changeCourse(move.amount))
    }
  }
}

case class Position(x: Int, y: Int) {

  def change(heading: Heading, amount: Int): Position = Position(x + heading.xComponent * amount, y + heading.yComponent * amount)

  def getManhattanDistance: Int = Math.abs(x) + Math.abs(y)
}

abstract class Heading() {
  val heading: Int
  val xComponent: Int
  val yComponent: Int

  def changeCourse(change: Int): Heading = {
    addHeadings(heading, change)
  }

  private def addHeadings(a: Int, b: Int): Heading = {
    var mod = (a + b) % 360
    if (mod < 0) mod += 360
    mod match {
      case 0 => North
      case 90 => East
      case 180 => South
      case 270 => West
    }
  }
}

case object North extends Heading {
  val heading = 0
  val xComponent = 0
  val yComponent = 1
}

case object East extends Heading {
  val heading = 90
  val xComponent = 1
  val yComponent = 0
}

case object South extends Heading {
  val heading = 180
  val xComponent = 0
  val yComponent = -1
}

case object West extends Heading {
  val heading = 270
  val xComponent = -1
  val yComponent = 0
}

case class Move(direction: Char, amount: Int)


