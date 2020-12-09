package adventOfCode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day9 extends App {
  val lines = Source.fromResource("day9Input.txt").getLines.toList
  val numbers = lines.map(_.toLong)
  val invalidNumbers = mutable.ListBuffer.empty[Long]
  val preambleLength = 25

  numbers.sliding(preambleLength + 1).foreach {
    case preamble :+ element => if (!calculateOptions(preamble).contains(element)) invalidNumbers.addOne(element)
  }
  // Part 1
  println(invalidNumbers.min)

  // Part 2
  val contiguousSet = findContiguousSet(numbers, 1, invalidNumbers.min)
  println(contiguousSet.min + contiguousSet.max)

  @tailrec
  def findContiguousSet(candidates: List[Long], endOfSlice: Int, number: Long): List[Long] = {
    if (endOfSlice >= candidates.length) findContiguousSet(candidates.tail, 1, number)
    else if (candidates.slice(0, endOfSlice).sum == number) candidates.slice(0, endOfSlice)
    else findContiguousSet(candidates, endOfSlice + 1, number)
  }

  def calculateOptions(numbers: List[Long]): List[Long] = for {
    numberA <- numbers
    numberB <- numbers
  } yield numberA + numberB
}
