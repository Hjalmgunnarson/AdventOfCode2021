package adventOfCode

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day14 extends App {
  val lines = Source.fromResource("day14Input.txt").getLines.toList
  val maskPattern = "mask = ([\\w]+)".r
  val memPattern = "mem\\[([\\d]+)] = ([\\d]+)".r
  val program = lines.map {
    case maskPattern(mask) => Mask(mask)
    case memPattern(address, number) => Mem(address.toLong, number.toLong)
  }.toArray

  // Part1
  val emptyMask = (0 until 36).map(_ => 'X').mkString
  val emptyMemory = Map.empty[Long, IntIsh]


  val lastComputerV1 = program.foldLeft(Computer(emptyMemory, Mask(emptyMask), 1))((computer, instruction) => computer.execute(instruction))
  println(lastComputerV1.memory.values.map(intIsh => intIshToLong(intIsh)).sum)

  // Part2
  val lastComputerV2 = program.foldLeft(Computer(emptyMemory, Mask(emptyMask), 2))((computer, instruction) => computer.execute(instruction))
  println(lastComputerV2.memory.values.map(intIsh => intIshToLong(intIsh)).sum)

  case class Computer(memory: Map[Long, IntIsh], currentMask: Mask, version: Int) {

    def execute(instruction: Instruction): Computer = instruction match {
      case mask: Mask => setMask(mask)
      case mem: Mem if version == 2 => computeV2(mem)
      case mem: Mem => computeV1(mem)
    }

    def setMask(mask: Mask): Computer = Computer(memory, mask, version)

    def computeV1(mem: Mem): Computer = {
      val memAndMask = mem.numberToIntIsh.zip(currentMask.getBitList)
      val newValue = memAndMask.map {
        case (_, Some(mask)) if mask => true
        case (_, Some(_)) => false
        case (value, None) => value
      }
      Computer(memory.updated(mem.address, newValue), currentMask, version)
    }

    def computeV2(mem: Mem): Computer = {
      val updatedMemory = currentMask.computeAddresses(mem).foldLeft(memory) {
        (memory, address) => memory.updated(address, mem.numberToIntIsh)
      }
      Computer(updatedMemory, currentMask, version)
    }
  }

  abstract class Instruction()

  case class Mask(mask: String) extends Instruction {

    def getBitList: Array[Option[Boolean]] = mask.toCharArray.map {
      case 'X' => None
      case '0' => Some(false)
      case '1' => Some(true)
    }

    // Solution taken from https://todd.ginsberg.com/post/advent-of-code/2020/day14/
    def computeAddresses(mem: Mem): List[Long] = {
      val addresses = ListBuffer(mem.addressToIntIsh)
      mask.zipWithIndex.foreach { entry =>
        entry._1 match {
          case '1' => addresses.foreach(a => a.update(entry._2, true)) // Set to one
          case 'X' => addresses.foreach(a => a.update(entry._2, true)) // Set to one
            addresses.addAll {
              addresses.map { // Create copies with zeroes
                address =>
                  val newAddress = address.clone()
                  newAddress.update(entry._2, false)
                  newAddress
              }
            }
          case _ => ()
        }
      }
      addresses.map(intIshToLong).toList
    }

    override def toString: String = mask
  }

  case class Mem(address: Long, number: Long) extends Instruction {

    def numberToIntIsh: IntIsh = longToIntIsh(number)

    def addressToIntIsh: IntIsh = longToIntIsh(address)

    override def toString: String = intIshString(numberToIntIsh)
  }

  type IntIsh = Array[Boolean]

  def intIshString(intIsh: IntIsh): String = intIsh.map {
    case false => '0'
    case true => '1'
  }.mkString

  def intIshToLong(list: IntIsh): Long = {
    val bits = list.map {
      case false => 0
      case true => 1
    }
    bits.reverse.zipWithIndex.map { case (bit, weight) => (scala.math.pow(2, weight) * bit).toLong }.sum
  }

  def longToIntIsh(number: Long): IntIsh = {

    def pad(bitList: Array[Boolean], length: Int): IntIsh = {
      val diff = length - bitList.length
      val appendix = (0 until diff).map(_ => false)
      bitList ++ appendix
    }

    val bitList = number.toBinaryString.reverse.map {
      case '0' => false
      case '1' => true
    }.toArray
    pad(bitList, 36).reverse
  }
}
