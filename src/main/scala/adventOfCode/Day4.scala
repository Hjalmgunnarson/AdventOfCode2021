package adventOfCode


import adventOfCode.Passport.{requiredFields, validEyeColours}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day4 extends App {
  val lines = Source.fromResource("day4Input.txt").getLines.toList
  val passports = convert(List.empty[Passport], Passport(), lines)
  val validPassports = passports.filter(_.valid())
  println(validPassports.length)

  @tailrec
  def convert(passports: List[Passport], passport: Passport, data: List[String]): List[Passport] = {
    data match {
      case Nil => passports :+ passport
      case line :: lines if line == "" => convert(passports :+ passport, Passport(), lines)
      case line :: lines => convert(passports, passport.add(line), lines)
    }
  }
}

object Passport {
  private val requiredFields = "byr" :: "iyr" :: "eyr" :: "hgt" :: "hcl" :: "ecl" :: "pid" :: Nil
  private val validEyeColours = "amb" :: "blu" :: "brn" :: "gry" :: "grn" :: "hzl" :: "oth" :: Nil
}

case class Passport() {
  private val entries = mutable.HashMap.empty[String, String]
  private val partPattern = "(.+):(.+)".r

  def add(parts: String): Passport = {
    parts.split("\\s").foreach(part => {
      val partPattern(field, value) = part
      entries.put(field, value)
    })
    this
  }

  def valid(): Boolean = {
    if (requiredFields.forall(entries.contains))
      entries.forall { case (field, value) => fieldValid(field, value) }
    else
      false
  }

  def fieldValid(field: String, value: String): Boolean = {
    field match {
      case "byr" => value.length == 4 && 1920 <= value.toInt && value.toInt <= 2002
      case "iyr" => value.length == 4 && 2010 <= value.toInt && value.toInt <= 2020
      case "eyr" => value.length == 4 && 2020 <= value.toInt && value.toInt <= 2030
      case "hgt" => validateHeight(value)
      case "hcl" => "(#[0-9a-f]{6})".r.matches(value)
      case "ecl" => validEyeColours.contains(value)
      case "pid" => "([0-9]{9})".r.matches(value)
      case "cid" => true
    }
  }

  def validateHeight(value: String): Boolean = {
    val pattern = "([0-9]+)(cm|in)".r
    if (pattern.matches(value)) {
      val pattern(length, unit) = value
      unit match {
        case "in" => 59 <= length.toInt && length.toInt <= 76
        case "cm" => 150 <= length.toInt && length.toInt <= 193
      }
    }
    else
      false
  }
}