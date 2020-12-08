package adventOfCode

import scala.collection.mutable
import scala.io.Source

object Day7 extends App {
  val lines = Source.fromResource("day7Input.txt").getLines.toList
  val allBags = mutable.HashMap.empty[String, Bag]

  val pattern1 = "(.*) bags contain (.*)".r
  val pattern2 = " ?(\\d) ([a-z ]+) bag[s]?[.,]".r
  for {
    line <- lines
    pattern1(outerBag, allContents) = line
    splitContent <- pattern2.findAllIn(allContents)
    pattern2(amount, innerBag) = splitContent
  } createOrUpdateBag(outerBag, innerBag, amount.toInt)

  // Part 1
  val candidateBags = allBags.values.filter(_.canContain(allBags("shiny gold")))
  println(candidateBags.size)

  // Part 2
  println(allBags("shiny gold").countContent())

  def createOrUpdateBag(outerBag: String, innerBag: String, amount: Int): Unit = {
    val oBag = allBags.getOrElseUpdate(outerBag, Bag(outerBag))
    val iBag = allBags.getOrElseUpdate(innerBag, Bag(innerBag))
    oBag.contents.put(iBag, amount)
  }
}

case class Bag(colour: String) {
  val contents = mutable.HashMap.empty[Bag, Int]

  def canContain(bag: Bag): Boolean = {
    contents.contains(bag) || contents.keys.exists(_.canContain(bag))
  }

  def countContent(): Int = contents.foldLeft(contents.values.sum) {
    // bags contained by this bag + their contents multiplied by their amount
    (acc, contentsEntry) => acc + (contentsEntry._2 * contentsEntry._1.countContent())
  }
}
