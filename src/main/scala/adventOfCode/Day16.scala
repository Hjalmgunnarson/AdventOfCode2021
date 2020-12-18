package adventOfCode

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day16 extends App {
  val allTickets = ListBuffer.empty[Ticket]
  val allRules = ListBuffer.empty[Rule]
  parse()

  val myTicket = allTickets.head
  val nearbyTickets = allTickets.tail

  // Part1
  val ticketScanningErrorRate = nearbyTickets.filter(!_.isValid(allRules.toList)).map(_.getSumInvalidValues(allRules.toList)).sum
  println(ticketScanningErrorRate)

  def parse(): Unit = {
    val lines = Source.fromResource("day16Input.txt").getLines.toList
    val ticketRulePattern = "([ \\w]+): ([ \\w-]*)".r
    val ticketPattern = "([\\d,]+)".r

    lines.foreach {
      case ticketRulePattern(ruleName, rules) =>
        val patterns = "([\\d-]+)".r.findAllIn(rules).toList
        allRules.addOne(Rule(ruleName, patterns))
      case ticketPattern(ticketContent) =>
        val values = "([\\d]+)".r.findAllIn(ticketContent).toList
        allTickets.addOne(Ticket(values))
      case _ => ()
    }
  }

  case class Ticket(values: Seq[String]) {
    override def toString: String = values.mkString(", ")

    def isValid(rules: List[Rule]): Boolean = values.forall(valueValid(rules, _))

    private def valueValid(rules: List[Rule], value: String): Boolean = rules.exists(_.isValidValue(value))

    def getSumInvalidValues(rules: List[Rule]): Int = values.foldLeft(0) {
      (acc, value) => if (valueValid(rules, value)) acc else acc + value.toInt
    }
  }

  case class Rule(name: String, patterns: List[String]) {
    private val regex = "([\\d]+)-([\\d]+)".r
    private val ranges: List[(Int, Int)] = patterns.map { case regex(lower, upper) => (lower.toInt, upper.toInt) }

    override def toString: String = name + ": " + patterns.mkString(", ")

    def isValidValue(value: String): Boolean = ranges.foldLeft(false) {
      (valid, rule) => valid || (rule._1 <= value.toInt && value.toInt <= rule._2)
    }
  }

}
