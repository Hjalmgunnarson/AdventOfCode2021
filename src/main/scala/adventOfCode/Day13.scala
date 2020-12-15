package adventOfCode

import scala.io.Source

object Day13 extends App {
  val lines = Source.fromResource("day13Input.txt").getLines.toList
  val time = lines.head.toInt
  val pattern = "([\\d]+)".r
  val linesVsArrivalTime = pattern.findAllIn(lines.tail.head).map(_.toInt).map(id => (((time / id) + 1) * id - time) -> id).toMap
  val earliest = linesVsArrivalTime.keys.min
  // Part 1
  println(linesVsArrivalTime(earliest) * earliest)


}
