package adventOfCode

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  val inputLines = Source.fromResource("day5Input.txt").getLines.toList
  val coordinates = inputLines.map(l => "\\d+".r.findAllIn(l).map(_.toInt).toList)
  val lines = coordinates.map {
    case xStart :: yStart :: xEnd :: yEnd :: Nil => Line(xStart, yStart, xEnd, yEnd)
  }
  val chart = createChart(Map.empty[Point, Int], lines, useDiagonals = false)
  println(chart.count { case (_, count) => count >= 2 })

  val diagonalChart = createChart(Map.empty[Point, Int], lines, useDiagonals = true)
  println(diagonalChart.count { case (_, count) => count >= 2 })

  @tailrec
  def createChart(map: Map[Point, Int], lines: List[Line], useDiagonals: Boolean): Map[Point, Int] = {
    lines match {
      case Nil => map
      case line :: lines if line.isDiagonal && !useDiagonals => createChart(map, lines, useDiagonals)
      case line :: lines =>
        val updatedMap = line.getIntermediatePoints.foldLeft(map) {
          case (map, point) => map.updatedWith(point) {
            _.map(_ + 1).orElse(Some(1))
          }
        }
        createChart(updatedMap, lines, useDiagonals)
    }
  }

  case class Line(xStart: Int, yStart: Int, xEnd: Int, yEnd: Int) {

    def isDiagonal: Boolean = xStart != xEnd && yStart != yEnd

    def getIntermediatePoints: List[Point] = {
      val xStep = if (xStart <= xEnd) 1 else -1
      val yStep = if (yStart <= yEnd) 1 else -1
      if (this.isDiagonal)
        (xStart to xEnd by xStep).zip(yStart to yEnd by yStep).map { case (x, y) => Point(x, y) }.toList
      else {
        (for {
          x <- xStart to xEnd by xStep
          y <- yStart to yEnd by yStep
        } yield Point(x, y)).toList
      }
    }
  }

  case class Point(x: Int, y: Int)
}