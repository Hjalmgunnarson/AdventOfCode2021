package adventOfCode

import scala.io.Source

object Day2 extends App {
  def xor(bool1: Boolean, bool2: Boolean): Boolean = (bool1 || bool2) && !(bool1 && bool2)

  val lines = Source.fromResource("day2Input.txt").getLines.toList
  //  line example: 15-16 f: ffffffffffffffhf
  val pattern = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]*)".r

  val validPasswords = for {
    line <- lines
    pattern(lowerLimit, upperLimit, letter, password) = line
    count = password.count(_ == letter(0))
    if lowerLimit.toInt <= count && count <= upperLimit.toInt
  } yield password

  println(validPasswords.length)
  println("++++++++++++++++")


  val validPasswords2 = for {
    line <- lines
    pattern(firstPos, secondPos, letter, password) = line
    charAtFirstPos = password(firstPos.toInt - 1)
    charAtSecondPos = password(secondPos.toInt - 1)
    if xor(charAtFirstPos == letter(0), charAtSecondPos == letter(0))
  } yield password

  println(validPasswords2.length)

}
