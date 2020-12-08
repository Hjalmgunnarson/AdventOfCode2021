package adventOfCode
import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {
  val lines = Source.fromResource("day3Input.txt").getLines.toArray
  val mountainPart = lines.map(_.toCharArray)
  // Part 1
  val numberOfTreesRun = rideTobbogan(Slope(3, 1), 0, 0, 0)
  println(numberOfTreesRun)

  // Part2
  val slopes = Slope(1,1) :: Slope(3,1) :: Slope(5,1) :: Slope(7,1) :: Slope(1,2) :: Nil
  val product = slopes.foldLeft(1) {
    (acc, slope) => acc * rideTobbogan(slope, 0, 0, 0)
  }
  println(product)

  @tailrec
  def rideTobbogan(slope: Slope, offsetX: Int, offsetY: Int, numberOfTrees: Int): Int = {
    if (offsetY >= mountainPart.length)
      return numberOfTrees
    if (offsetX >= mountainPart(0).length)
      rideTobbogan(slope, offsetX - mountainPart(0).length, offsetY, numberOfTrees)
    else {
      mountainPart(offsetY)(offsetX) match {
        case '.' => rideTobbogan(slope, offsetX + slope.stepX, offsetY + slope.stepY, numberOfTrees)
        case '#' => rideTobbogan(slope, offsetX + slope.stepX, offsetY + slope.stepY, numberOfTrees + 1)
      }
    }
  }
}

case class Slope(stepX: Int, stepY: Int)