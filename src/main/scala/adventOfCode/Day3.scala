package adventOfCode

import scala.collection.mutable
import scala.io.Source

object Day3 extends App {
  val test = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
  val lines = Source.fromResource("day3Input.txt").getLines.toList

  val bitMap = transpose(lines)
  val gammaRateBits = getGammaRate(bitMap)
  val epsilonRateBits = getEpsilonRateBits(gammaRateBits)
  val gammaRate = toDecimalValue(gammaRateBits)
  val epsilonRate = toDecimalValue(epsilonRateBits)
  println(gammaRate * epsilonRate)

  // for each bit index, return the most common bit value
  def getGammaRate(bitSequences: Map[Int, Seq[Int]]): Map[Int, Int] = {
    bitSequences.map { case (index, sequence) =>
      val ones = sequence.count(_ == 1)
      val zeroes = sequence.count(_ == 0)
      val value = if (ones > zeroes) 1 else 0
      (index, value)
    }
  }

  // invert the gammaRate bits
  def getEpsilonRateBits(gammaRateBits: Map[Int, Int]): Map[Int, Int] =
    gammaRateBits.map { case (index, value) => if (value == 0) (index, 1) else (index, 0) }

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
