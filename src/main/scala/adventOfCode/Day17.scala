package adventOfCode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day17 extends App {

  val allCubes = parse()

  solve1(0, 6, allCubes)

  @tailrec
  def solve1(increment: Int, stopAt: Int, allCubes: Map[Cube, Boolean]): Map[Cube, Boolean] = {
    println(increment + " -> " + allCubes.values.count(_ == true))
    val cubesToConsider = allCubes.keys.flatMap(_.getNeighbours).toSet // Remove duplicates by using set
    val newState = cubesToConsider.map { cube =>
      val neighboursActive = cube.getNeighbours.map {
        allCubes.getOrElse(_, false)
      }.count(_ == true)
      val thisCubeState = allCubes.getOrElse(cube, false)
      if ((neighboursActive == 2 || neighboursActive == 3) && thisCubeState) cube -> true
      else if (neighboursActive == 3 && !thisCubeState) cube -> true
      else cube -> false
    }.toMap
    if (increment == stopAt) newState
    else solve1(increment + 1, stopAt, newState)
  }

  def parse(): Map[Cube, Boolean] = {
    val allCubes = mutable.HashMap.empty[Cube, Boolean]
    val data = Source.fromResource("day17Input.txt").getLines.toList
    for {
      (line, y) <- data.zipWithIndex
      (char, x) <- line.zipWithIndex
    } {
      val active = char match {
        case '#' => true
        case '.' => false
      }
      allCubes.update(Cube(x, y), active)
    }
    allCubes.toMap
  }
}

case class Cube(x: Int, y: Int, z: Int = 0) {

  def getNeighbours: Seq[Cube] = for {
    z <- -1 to 1
    y <- -1 to 1
    x <- -1 to 1
    if z != 0 || y != 0 || x != 0
  } yield Cube(this.x + x, this.y + y, this.z + z)
}

