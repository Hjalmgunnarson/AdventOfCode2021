package adventOfCode

import scala.io.Source

object Day13 extends App {
  val lines = Source.fromResource("day13Input.txt").getLines.toList
  val time = lines.head.toInt
  val pattern = "([\\w]+)".r
  val busMap = pattern.findAllIn(lines.tail.head).zipWithIndex.toMap.filter(_._1 != "x")
  val linesVsArrivalTime = busMap.keys.map(_.toInt).map(id => (((time / id) + 1) * id - time) -> id).toMap
  val earliest = linesVsArrivalTime.keys.min
  // Part 1
  println(linesVsArrivalTime(earliest) * earliest)

  val busList = busMap.map(entry => Bus(entry._2, entry._1.toInt)).toList.sortBy(_.offset)
  //Part 2
  println(solvePart2(busList))

  // Solution taken from https://todd.ginsberg.com/post/advent-of-code/2020/day13/
  def solvePart2(busList: List[Bus]): Long = {
    var stepSize = busList.head.id.toLong
    var time: Long = 0L
    busList.tail.foreach { bus =>
      while ((time + bus.offset) % bus.id != 0L)
        time += stepSize
      stepSize *= bus.id // New Ratio!
    }
    time
  }
  case class Bus(offset: Int, id: Int)

}
