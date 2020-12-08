package adventOfCode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day6 extends App {
  val lines = Source.fromResource("day6Input.txt").getLines.toList
  val groups = convert(List.empty[Group], Group(), lines)
  // Part 1
  println(groups.map(_.getUniqueAnsweredQuestions).sum)
  // Part 2
  println(groups.map(_.getQuestionsAnsweredByAll).sum)

  @tailrec
  def convert(groups: List[Group], group: Group, questions: List[String]): List[Group] =
    questions match { // Re-use of day 4
      case Nil => groups :+ group
      case line :: lines if line == "" => convert(groups :+ group, Group(), lines)
      case questions :: lines => convert(groups, group.add(questions), lines)
    }
}

case class Group() {
  private val questionsAnsweredYes = mutable.ListBuffer.empty[Char]
  private val questionsAnsweredPerPerson = mutable.ListBuffer.empty[Array[Char]]

  def add(questions: String): Group = {
    questions.toCharArray.foreach(questionsAnsweredYes.addOne)
    questionsAnsweredPerPerson.addOne(questions.toCharArray)
    this
  }
  def getUniqueAnsweredQuestions: Int = questionsAnsweredYes.distinct.length
  def getQuestionsAnsweredByAll: Int = questionsAnsweredPerPerson.
    foldLeft (questionsAnsweredPerPerson.head)( (a, b) => a.intersect(b)).length
}