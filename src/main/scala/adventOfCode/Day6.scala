package adventOfCode

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {
  val test = List(3, 4, 3, 1, 2).map(LanternFish)

  val lines = Source.fromResource("day6Input.txt").getLines.toList
  val fish = "\\d+".r.findAllIn(lines.head).map(_.toInt).map(LanternFish).toList

  val days = (1 to 80).toList

  println(multiply(days, fish).size)

  @tailrec
  def multiply(days: List[Int], lanternFish: List[LanternFish]): List[LanternFish] = {
    days match {
      case Nil => lanternFish
      case day :: days => // println(day - 1 + " "  + lanternFish.mkString(","))
        val newFish = lanternFish.flatMap { fish => fish.tick() }
        multiply(days, lanternFish ++ newFish)
    }
  }
}

case class LanternFish(var timer: Int = 8) {

  def tick(): Option[LanternFish] = {
    timer = timer - 1
    if (timer < 0) {
      timer = 6
      Some(LanternFish())
    } else None
  }

  override def toString: String = timer.toString

}