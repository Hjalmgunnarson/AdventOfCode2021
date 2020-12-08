package adventOfCode

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

object Day8 extends App {
  val lines = Source.fromResource("day8Input.txt").getLines.toList
  val stack = mutable.Stack.empty[(Int, Instruction)]

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
      case Some(instruction) if instruction.executed => traceBack(accumulator)
      case Some(instruction) =>
        stack.push((address, instruction))
        val result = instruction.execute()
        run(address + result.newOffset, accumulator + result.result)
    }

  @tailrec
  def traceBack(accumulator: Int): Int =
    stack.pop match {
      case (_, inst) if instructions.exists(_.isAltered) && !inst.isAltered => // Go back to previous change
        // Undo changes made by this instruction to the accumulator
        inst.executed = false
        traceBack(accumulator - inst.execute().result)
      case (_, inst) if instructions.exists(_.isAltered) => // Found previous change
        // This instruction was changed in a previous attempt. Undo change and look for the previous jmp or nop
        inst.alter()
        traceBack(accumulator)
      case (address, inst) if !inst.isAcc && !inst.isAltered => // Found new candidate for change
        // This instruction was never changed. Change and try program
        inst.alter()
        run(address, accumulator)
      case (_, inst) => // Undo changes
        // Undo changes made by this instruction to the accumulator
        inst.executed = false
        traceBack(accumulator - inst.execute().result)
    }
}

case class Instruction(var mnemonic: String, argument: Int, var executed: Boolean = false, var altered: Boolean = false) {

  def execute(): Result = {
    executed = true
    mnemonic match {
      case "acc" => Result(argument, 1)
      case "jmp" => Result(0, argument)
      case "nop" => Result(0, 1)
    }
  }

  def isAcc: Boolean = mnemonic == "acc"

  def isAltered: Boolean = altered

  def alter(): Unit = {
    executed = false
    altered = !altered
    if (mnemonic == "jmp") mnemonic = "nop"
    else mnemonic = "jmp"
  }
}

case class Result(result: Int, newOffset: Int)

