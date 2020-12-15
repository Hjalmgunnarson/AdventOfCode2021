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

  val finalShipWithWaypoint = moves.foldLeft(ShipWithWaypoint(Position(0, 0), Position(10, 1)))((ship, move) => ship.sail(move))
  // Part 1
  println(finalShipWithWaypoint.position.getManhattanDistance)

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

case class ShipWithWaypoint(position: Position, waypoint: Position) {

  def sail(move: Move): ShipWithWaypoint = {
    move.direction match {
      case 'N' => ShipWithWaypoint(position, waypoint.change(North, move.amount))
      case 'E' => ShipWithWaypoint(position, waypoint.change(East, move.amount))
      case 'S' => ShipWithWaypoint(position, waypoint.change(South, move.amount))
      case 'W' => ShipWithWaypoint(position, waypoint.change(West, move.amount))
      case 'F' => ShipWithWaypoint(position.change(waypoint, move.amount), waypoint)
      case 'L' => ShipWithWaypoint(position, waypoint.rotate(move))
      case 'R' => ShipWithWaypoint(position, waypoint.rotate(move))
    }
  }
}

case class Position(x: Int, y: Int) {

  def change(heading: Heading, amount: Int): Position = Position(x + heading.xComponent * amount, y + heading.yComponent * amount)

  def change(waypoint: Position, amount: Int): Position = Position(x + amount * waypoint.x, y + amount * waypoint.y)

  def getManhattanDistance: Int = Math.abs(x) + Math.abs(y)

  def rotate(move: Move): Position = {
    move.direction match {
      case 'R' if move.amount == 90 => Position(y, -x)
      case 'R' if move.amount == 180 => Position(-x, -y)
      case 'R' if move.amount == 270 => Position(-y, x)
      case 'L' if move.amount == 90 => Position(-y, x)
      case 'L' if move.amount == 180 => Position(-x, -y)
      case 'L' if move.amount == 270 => Position(y, -x)
    }
  }
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

