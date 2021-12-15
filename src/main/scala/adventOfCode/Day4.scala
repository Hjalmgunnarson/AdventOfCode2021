package adventOfCode

import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {

  val lines = Source.fromResource("day4Input.txt").getLines.toList
  val bingoNumbers = "\\d+".r.findAllIn(lines.head).map(_.toInt).toSeq

  val cards = createCards(List.empty[BingoCard], Seq.empty[Seq[Int]], lines.tail.tail)

  println(play(cards, bingoNumbers).get)

  def play(cards: List[BingoCard], numbers: Seq[Int]): Option[Int] = {
    numbers match {
      case Nil => Option.empty
      case number :: numbers =>
        cards.foreach(card => card.check(number))
        cards.find(_.bingo()).map(_.getPoints * number).orElse(play(cards, numbers))
    }
  }

  @tailrec
  def createCards(cards: List[BingoCard], parsedRows: Seq[Seq[Int]], input: List[String]): List[BingoCard] = {
    input match {
      case Nil => cards :+ BingoCard(parsedRows)
      case line :: lines if line == "" => createCards(cards :+ BingoCard(parsedRows), Seq.empty[Seq[Int]], lines)
      case line :: lines =>
        createCards(cards, parsedRows :+ "\\d+".r.findAllIn(line).map(_.toInt).toSeq, lines)
    }
  }
}

case class BingoCard(rows: Seq[Seq[Int]]) {
  val squareRows: Seq[Seq[Square]] = rows.map {
    _.map(Square(_, checked = false))
  }

  def squareColumns: Seq[Seq[Square]] = squareRows.transpose

  def check(number: Int): Unit = squareRows.foreach {
    squares => squares.find(s => s.value == number).foreach(s => s.checked = true)
  }

  def bingo(): Boolean = {
    squareRows.exists { squares => squares.forall(_.checked) } ||
      squareColumns.exists { squares => squares.forall(_.checked) }
  }

  def getPoints: Int =
    squareRows.flatMap { squares => squares.collect { case square if !square.checked => square.value } }.sum

  case class Square(value: Int, var checked: Boolean)
}