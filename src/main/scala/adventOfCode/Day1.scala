package adventOfCode

import scala.io.Source

object Day1 extends App {
  val test = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  val numbers = Source.fromResource("day1Input.txt").getLines.toList.map(_.toInt)
  // Part 1
  println("Part 1: " + countIncreasingMeasurements(numbers))

  // Part 2
  val summed = numbers.iterator.sliding(3).withPartial(false).map(_.sum).toList
  println("Part 2: " + countIncreasingMeasurements(summed))

  def countIncreasingMeasurements(list: List[Int]): Int =
    list.sliding(2).map { case a :: b :: Nil => if (b > a) 1 else 0 }.sum

}
