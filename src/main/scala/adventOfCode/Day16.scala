package adventOfCode

import scala.annotation.tailrec
import scala.collection.mutable
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

  //Part2
  val validTickets = nearbyTickets.filter(_.isValid(allRules.toList))
  validTickets.foreach(_.identifyValues(allRules.toList))

  val fieldLabelListsMap = mutable.HashMap.empty[Int, List[List[String]]]
  for {
    ticket <- validTickets
    (index, fieldLabels) <- ticket.fieldLabels
  } fieldLabelListsMap.updateWith(index) {
    case Some(list) => Some(list :+ fieldLabels)
    case None => Some(List(fieldLabels))
  }

  val intersectedLists = fieldLabelListsMap.map { case (index, list) => index -> list.foldLeft(list.head)((acc, list) => acc.intersect(list)) }
  val fieldMapping = reduceOptions(intersectedLists)

  val myTicketValues = fieldMapping.filter { case (_, oneItem) => oneItem.head.contains("departure") }.map { case (index, _) => myTicket.values(index).toLong }
  println(myTicketValues.product)

  @tailrec
  def reduceOptions(listsMap: mutable.HashMap[Int, List[String]]): mutable.HashMap[Int, List[String]] = {
    if (listsMap.forall { case (_, list) => list.length == 1 }) listsMap // Done
    else {
      val entriesWithOneItem = listsMap.filter { case (_, list) => list.length == 1 } // Identify lists that contain only one value
      entriesWithOneItem.foreach { case (indexToKeep, oneItem) =>
        println("Looking at " + oneItem.head + " @ " + indexToKeep)
        listsMap.foreach { case (index, _) if index != indexToKeep => listsMap.updateWith(index) {
          case Some(list) =>
            println("Trying to remove " + oneItem.head + " from " + list.mkString(", ") + " @" + index)
            Some(list.filterNot(_ == oneItem.head)) // Eliminate the value found from all other lists
          case None => None
        }
        case _ => ()
        }
      }
      reduceOptions(listsMap)
    }
  }

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

    val fieldLabels: mutable.HashMap[Int, List[String]] = mutable.HashMap.empty

    def isValid(rules: List[Rule]): Boolean = values.forall(valueValid(rules, _))

    private def valueValid(rules: List[Rule], value: String): Boolean = rules.exists(_.isValidValue(value))

    def getSumInvalidValues(rules: List[Rule]): Int = values.foldLeft(0) {
      (acc, value) => if (valueValid(rules, value)) acc else acc + value.toInt
    }

    def identifyValues(rules: List[Rule]): Unit = {
      values.zipWithIndex.foreach { case (value, index) =>
        rules.foreach { rule =>
          if (rule.isValidValue(value)) {
            fieldLabels.updateWith(index) {
              case Some(list) => Some(list :+ rule.name)
              case None => Some(List(rule.name))
            }
          }
        }
      }
      fieldLabels.foreach { case (index, values) => if (values.length == 1) println(index + " " + values.mkString(", ")) }
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
