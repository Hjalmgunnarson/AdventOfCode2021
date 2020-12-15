package adventOfCode

import scala.io.Source

object Day14 extends App {
  val lines = Source.fromResource("day14Input.txt").getLines.toList
  val maskPattern = "mask = ([\\w]+)".r
  val memPattern = "mem\\[([\\d]+)] = ([\\d]+)".r
  val program = lines.map {
    case maskPattern(mask) => Mask(mask)
    case memPattern(address, number) => Mem(address.toLong, number.toLong)
  }.toArray

  val emptyMask = (0 until 36).map(_ => 'X').toString()
  val lastComputer = program.foldLeft(Computer(Map.empty[Long, IntIsh], Mask(emptyMask)))((computer, instruction) => computer.execute(instruction))
  println(lastComputer.memory.values.map(intIsh => toLong(intIsh)).sum)

  type IntIsh = Array[Boolean]

  case class Computer(memory: Map[Long, IntIsh], currentMask: Mask) {

    def setMask(mask: Mask): Computer = Computer(memory, mask)

    def compute(mem: Mem): Computer = {
      val memAndMask = mem.getBitList.zip(currentMask.getBitList)
      val newValue = memAndMask.map {
        case (_, Some(mask)) if mask => true
        case (_, Some(mask)) => false
        case (value, None) => value
      }
      Computer(memory.updated(mem.address, newValue), currentMask)
    }

    def execute(instruction: Instruction): Computer = instruction match {
      case mask: Mask => setMask(mask)
      case mem: Mem => compute(mem)
    }
  }

  def toLong(list: IntIsh): Long = {
    val bits = list.map {
      case false => 0
      case true => 1
    }
    bits.reverse.zipWithIndex.map { case (bit, weight) => (scala.math.pow(2, weight) * bit).toLong }.sum
  }

  abstract class Instruction()

  case class Mask(mask: String) extends Instruction {

    def getBitList: Array[Option[Boolean]] = mask.toCharArray.map {
      case 'X' => None
      case '0' => Some(false)
      case '1' => Some(true)
    }

    override def toString: String = mask
  }

  case class Mem(address: Long, number: Long) extends Instruction {

    /**
     * @return Most Significant bit has index 0
     */
    def getBitList: IntIsh = {
      val bitList = number.toBinaryString.reverse.toCharArray.map {
        case '0' => false
        case '1' => true
      }
      pad(bitList).reverse
    }

    private def pad(bitList: Array[Boolean]): IntIsh = {
      val diff = 36 - bitList.length
      val appendix = (0 until diff).map(_ => false)
      bitList ++ appendix
    }

    override def toString: String = intIshString(getBitList)
  }

  def intIshString(intIsh: IntIsh): String = intIsh.map {
    case false => '0'
    case true => '1'
  }.mkString


}
