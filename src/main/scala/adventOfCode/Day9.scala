package adventOfCode

import scala.io.Source

object Day9 extends App {
  val lines = Source.fromResource("day9Input.txt").getLines.toList
  val numbers = lines.map(_.toLong)

  val preambleLength = 25

  numbers.sliding(preambleLength + 1).foreach {
    case preamble :+ element => if (!calculateOptions(preamble).contains(element)) println(element)
  }

  def calculateOptions(numbers: List[Long]): List[Long] = for {
    numberA <- numbers
    numberB <- numbers
  } yield numberA + numberB
}

