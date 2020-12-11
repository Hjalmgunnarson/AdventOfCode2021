package adventOfCode

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {
  type SeatMap = Map[Int, Map[Int, Spot]]
  val lines = Source.fromResource("day11Input.txt").getLines.toArray
  val seatMap: SeatMap = lines.zipWithIndex.map(y => y._2 -> y._1.toCharArray.zipWithIndex.map(x => x._2 -> Spot(x._1)).toMap).toMap

  val directions = for {
    y <- -1 to 1
    x <- -1 to 1
    if y != 0 || x != 0
  } yield (x, y)

  val part1 = transform(seatMap, 4, findDirectNeighbours)
  println(countOccupiedSeats(part1))

  val part2 = transform(seatMap, 5, findVisibleSeats)
  println(countOccupiedSeats(part2))

  @tailrec
  def transform(spots: SeatMap, maxOccupied: Int, findSeats: (Int, Int, SeatMap) => Seq[Spot]): SeatMap = {
    var changed = false
    val newArrangement = spots.map { yEntry =>
      yEntry._1 -> yEntry._2.map { xEntry =>
        val neighboursOccupiedAmount = findSeats(xEntry._1, yEntry._1, spots).count(_.isOccupied)

        if (xEntry._2.isEmptySeat && neighboursOccupiedAmount == 0) {
          changed = true
          xEntry._1 -> Spot('#')
        }
        else if (xEntry._2.isOccupied && neighboursOccupiedAmount >= maxOccupied) {
          changed = true
          xEntry._1 -> Spot('L')
        }
        else xEntry._1 -> Spot(xEntry._2.char)
      }
    }
    if (!changed) newArrangement
    else transform(newArrangement, maxOccupied, findSeats)
  }

  def findDirectNeighbours(x: Int, y: Int, spots: SeatMap): Seq[Spot] = {
    directions.flatMap { coord =>
      val newX = x + coord._1
      val newY = y + coord._2
      getSpot(newX, newY, spots)
    }
  }

  def findVisibleSeats(x: Int, y: Int, spots: SeatMap): Seq[Spot] = {
    directions.flatMap(coord => findVisibleSeats(x + coord._1, y + coord._2, coord._1, coord._2, spots))
  }

  @tailrec
  def findVisibleSeats(x: Int, y: Int, slopeX: Int, slopeY: Int, spots: SeatMap): Option[Spot] = {
    val nextX = x + slopeX
    val nextY = y + slopeY

    getSpot(x, y, spots) match {
      case Some(spot) if !spot.isFloor => Some(spot)
      case None => None
      case _ => findVisibleSeats(nextX, nextY, slopeX, slopeY, spots)
    }
  }

  def getSpot(x: Int, y: Int, spots: SeatMap): Option[Spot] = spots.get(y).flatMap(_.get(x))

  def countOccupiedSeats(spots: SeatMap): Int =
    spots.values.foldLeft(0) { (acc, line) =>
      line.values.foldLeft(acc) {
        case (innerAcc, spot) if spot.isOccupied => innerAcc + 1
        case (innerAcc, _) => innerAcc
      }
    }

  case class Spot(char: Char) {
    def isEmptySeat: Boolean = char == 'L'

    def isOccupied: Boolean = char == '#'

    def isFloor: Boolean = char == '.'

  }
}
