package adventOfCode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day10 extends App {

  val lines = Source.fromResource("day10Input.txt").getLines.toList
  val bagAdapters = lines.map(_.toInt)
  val joltDifferences = mutable.HashMap.empty[Int, Int]
  val maxJoltDiff = 3
  val adapters = bagAdapters.appended(bagAdapters.max + 3)

  // Part 1
  process(0, adapters.sorted)
  println(joltDifferences(joltDifferences.keys.min) * joltDifferences(joltDifferences.keys.max))

  // Part 2
  println(countPossibleCombinations(bagAdapters.sorted))

  @tailrec
  def process(joltageLevel: Int, adapters: List[Int]): Unit = {
    if (adapters.isEmpty) return
    val selectedAdapter = adapters.takeWhile(_ <= joltageLevel + maxJoltDiff).min
    val diff = selectedAdapter - joltageLevel

    joltDifferences.updateWith(diff) {
      case Some(v) => Some(v + 1)
      case None => Some(1)
    }
    process(selectedAdapter, adapters.filter(_ != selectedAdapter))
  }

  // Solution taken from https://todd.ginsberg.com/post/advent-of-code/2020/day10/
  def countPossibleCombinations(adapters: List[Int]): Long = {
    val pathsByAdapter = mutable.HashMap(0 -> 1L)

    adapters.foreach { adapter =>
      val combinations = (1 to 3).map(lookback => pathsByAdapter.getOrElse(adapter - lookback, 0L)).sum
      pathsByAdapter.put(adapter, combinations)
    }
    pathsByAdapter(bagAdapters.max)
  }
}
