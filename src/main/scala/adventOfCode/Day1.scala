package adventOfCode

import scala.io.Source

object Day1 extends App {
  val numbers = Source.fromResource("day1Input.txt").getLines.toList.map(_.toInt)

  println(numbers.sliding(2).map{ case a :: b :: Nil => if (b > a)  1 else 0 }.sum)

}
