package adventOfCode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day17 extends App {

  val allCubes = parse(Cube.create)
  val allHyperCubes = parse(HyperCube.create)

  solve(0, 6, allCubes)
  println()
  solve(0, 6, allHyperCubes)


  @tailrec
  def solve(increment: Int, stopAt: Int, allCubes: Map[AbstractCube, Boolean]): Map[AbstractCube, Boolean] = {
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
    else solve(increment + 1, stopAt, newState)
  }

  def parse(createCube: (Int, Int) => AbstractCube): Map[AbstractCube, Boolean] = {
    val allCubes = mutable.HashMap.empty[AbstractCube, Boolean]
    val data = Source.fromResource("day17Input.txt").getLines.toList
    for {
      (line, y) <- data.zipWithIndex
      (char, x) <- line.zipWithIndex
    } {
      val active = char match {
        case '#' => true
        case '.' => false
      }
      allCubes.update(createCube(x, y), active)
    }
    allCubes.toMap
  }
}

abstract class AbstractCube {
  def create(x: Int, y: Int): AbstractCube
  def getNeighbours: Seq[AbstractCube]
}

object Cube {
  def create(x: Int, y: Int): Cube = Cube(x, y)
}

case class Cube(x: Int, y: Int, z: Int = 0) extends AbstractCube {

  override def getNeighbours: Seq[Cube] = for {
    z <- -1 to 1
    y <- -1 to 1
    x <- -1 to 1
    if z != 0 || y != 0 || x != 0
  } yield Cube(this.x + x, this.y + y, this.z + z)

  override def create(x: Int, y: Int): AbstractCube = Cube(x, y)
}

object HyperCube {
  def create(x: Int, y: Int): HyperCube = HyperCube(x, y)
}

case class HyperCube(x: Int, y: Int, z: Int = 0, w: Int =0) extends AbstractCube {

  override def getNeighbours: Seq[HyperCube] = for {
    w <- -1 to 1
    z <- -1 to 1
    y <- -1 to 1
    x <- -1 to 1
    if z != 0 || y != 0 || x != 0 || w != 0
  } yield HyperCube(this.x + x, this.y + y, this.z + z, this.w + w)

  override def create(x: Int, y: Int): AbstractCube = HyperCube(x, y)
}