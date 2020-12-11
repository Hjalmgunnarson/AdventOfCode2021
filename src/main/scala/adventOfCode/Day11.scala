package adventOfCode

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {

  val lines = Source.fromResource("day11Input.txt").getLines.toArray
  val spots = lines.zipWithIndex.map(y => y._2 -> y._1.toCharArray.zipWithIndex.map(x => x._2 -> Spot(x._1)).toMap).toMap

  val neighbours = for {
    y <- -1 to 1
    x <- -1 to 1
    if y != 0 || x != 0
  } yield (x, y)

  val temp = transform(spots)
  println(countOccupiedSeats(temp))

  @tailrec
  def transform(spots: Map[Int, Map[Int, Spot]]): Map[Int, Map[Int, Spot]] = {
    var changed = false
    val newArrangement = spots.map { y =>
      y._1 -> y._2.map { x =>
        val neighboursOccupiedAmount = neighbours.foldLeft(0) { (acc, neighbourCoordinate) =>
          val newX = x._1 + neighbourCoordinate._1
          val newY = y._1 + neighbourCoordinate._2

          val spot = spots.get(newY).flatMap( _.get(newX))
          spot match {
            case Some(spot) if spot.isOccupied => acc + 1
            case _ => acc
          }
        }
        if (x._2.isEmptySeat && neighboursOccupiedAmount == 0) {
          changed = true
          x._1 -> Spot('#')
        }
        else if (x._2.isOccupied && neighboursOccupiedAmount >= 4) {
          changed = true
          x._1 -> Spot('L')
        }
        else x._1 -> Spot(x._2.char)
      }
    }
    if (!changed) newArrangement
    else transform(newArrangement)
  }

  def countOccupiedSeats(spots: Map[Int, Map[Int, Spot]]): Int = {
    spots.values.foldLeft(0) { (acc, line) =>
      line.values.foldLeft(acc) {
        case (innerAcc, spot) if spot.isOccupied => innerAcc + 1
        case (innerAcc, _) => innerAcc
      }
    }
  }

  case class Spot(char: Char) {
    def isEmptySeat: Boolean = char == 'L'

    def isOccupied: Boolean = char == '#'

    def isFloor: Boolean = char == '.'

    def print(): Unit = System.out.print(char)
  }

}
