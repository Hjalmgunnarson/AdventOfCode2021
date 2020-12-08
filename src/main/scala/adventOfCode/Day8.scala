package adventOfCode

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  val lines = Source.fromResource("day8Input.txt").getLines.toList
  val pattern = "([\\w]{3}) ([+-][\\d]+)".r
  val instructions = for {
    line <- lines
    pattern(mnemonic, argument) = line
  } yield Instruction(mnemonic, argument.toInt)

  println(run(0, 0))

  @tailrec
  def run(address: Int, accumulator: Int): Int =

    instructions.lift(address) match {

      case None => accumulator
      case Some(instruction) if instruction.executed => accumulator
      case Some(instruction) =>
        val result = instruction.execute()
        run(address + result.newOffset, accumulator + result.result)
    }
}

case class Instruction(mnemonic: String, argument: Int, var executed: Boolean = false) {

  def execute(): Result = {
    executed = true
    mnemonic match {
      case "acc" => Result(argument, 1)
      case "jmp" => Result(0, argument)
      case "nop" => Result(0, 1)
    }
  }
}

case class Result(result: Int, newOffset: Int)

