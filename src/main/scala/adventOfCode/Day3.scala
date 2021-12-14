package adventOfCode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day3 extends App {
  val test = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
  val lines = Source.fromResource("day3Input.txt").getLines.toList

  val bitMap = transpose(lines)
  val gammaRateBits = getRate(bitMap, mostCommon)
  val epsilonRateBits = getRate(bitMap, leastCommon)
  val gammaRate = toDecimalValue(gammaRateBits)
  val epsilonRate = toDecimalValue(epsilonRateBits)
  println(gammaRate * epsilonRate)


  // Part 2 ----------------------------------------------------------------
  val oxGenRatingBits = filterForRating(lines, oxygenBitCriteria, 0)
  val co2ScrubberRatingBits = filterForRating(lines, co2ScrubberCriteria, 0)
  val oxGenRating = toDecimalValue(oxGenRatingBits.head.zipWithIndex.map{ case (c, i) => i -> c.asDigit }.toMap)
  val co2ScrubberRating = toDecimalValue(co2ScrubberRatingBits.head.zipWithIndex.map{ case (c, i) => i -> c.asDigit }.toMap)
  println(oxGenRating * co2ScrubberRating)

  // for each bit index, return a bit determined by the criteria
  def getRate(bitSequences: Map[Int, Seq[Int]], criteria: (Int, Int) => Int): Map[Int, Int] = {
    bitSequences.map { case (index, sequence) =>
      val ones = sequence.count(_ == 1)
      val zeroes = sequence.count(_ == 0)
      (index, criteria(ones, zeroes))
    }
  }

  @tailrec
  def filterForRating(input: List[String], criteria: (Int, Int) => Int, index: Int): List[String] = {
    input match {
      case filteredValue :: Nil => List(filteredValue)
      case list =>
        val ones = list.count(s => s.charAt(index).asDigit == 1)
        val zeroes = list.count(s => s.charAt(index).asDigit == 0)
        filterForRating(list.filter(s => s.charAt(index).asDigit == criteria(ones, zeroes)), criteria, index + 1)
    }
  }

  def mostCommon(ones: Int, zeroes: Int): Int = if (ones > zeroes) 1 else 0

  def leastCommon(ones: Int, zeroes: Int): Int = if (ones > zeroes) 0 else 1

  def oxygenBitCriteria(ones: Int, zeroes: Int): Int = if (ones >= zeroes) 1 else 0

  def co2ScrubberCriteria(ones: Int, zeroes: Int): Int = if (ones >= zeroes) 0 else 1


  // binary to decimal conversion
  def toDecimalValue(bits: Map[Int, Int]): Int = bits.foldLeft(0) {
    case (acc, (index, bitValue)) => acc + Math.pow(2, bits.size - index - 1).toInt * bitValue
  }

  // Create a map from bit index -> sequence of bits with that index
  def transpose(list: List[String]): Map[Int, Seq[Int]] = {
    val map: mutable.HashMap[Int, Seq[Int]] = mutable.HashMap.empty[Int, Seq[Int]]
    for {
      entry <- list
      index <- 0 until entry.length
      bit = entry.charAt(index).asDigit
      mapContent = map.getOrElse(index, Seq.empty)
    } map.put(index, mapContent :+ bit)
    map.toMap
  }
}
