package adventOfCode

import scala.io.Source

object Day1 extends App {
  val numbers = Source.fromResource("day1Input.txt").getLines.toList
  for {
    numberA <- numbers.map(_.toInt)
    numberB <- numbers.map(_.toInt)
    if numberA + numberB == 2020
    product = numberA * numberB
  } println(product)

  for {
    numberA <- numbers.map(_.toInt)
    numberB <- numbers.map(_.toInt)
    numberC <- numbers.map(_.toInt)
    if numberA + numberB + numberC == 2020
    product = numberA * numberB * numberC
  } println(product)
}
