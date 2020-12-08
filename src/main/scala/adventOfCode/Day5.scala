package adventOfCode

import scala.io.Source

object Day5 extends App {
  val passes = Source.fromResource("day5Input.txt").getLines.toList
  val pattern = "([FB]{7})([LR]{3})".r
  val seatIDs = for {
    pass <- passes
    pattern(rowNumber, columnNumber) = pass
  } yield BoardingPass(rowNumber, columnNumber).getSeatNumber
  // Part 1
  println(seatIDs.max)

  // Part 2
  seatIDs.sorted.sliding(2).foreach {
    case List(previous, current) if current - previous > 1 => println(current - 1)
    case _ => ()
  }


  case class BoardingPass(rowString: String, columnString: String) {
    private def convertDigit(c: Char): Int = c match {
      case 'F' => 0
      case 'B' => 1
      case 'L' => 0
      case 'R' => 1
    }

    private def toInt(string: String): Int = {
      val bits = string.toCharArray.map(convertDigit)
      bits.reverse.zipWithIndex.map { case (bit, weight) => (scala.math.pow(2, weight) * bit).toInt }.sum
    }

    def getRowNumber: Int = toInt(rowString)

    def getColumnNumber: Int = toInt(columnString)

    def getSeatNumber: Int = (getRowNumber * 8) + getColumnNumber
  }

}
