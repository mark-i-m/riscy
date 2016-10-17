package riscy

import Chisel._

// An extended version of the decoded instruction with the same
// signals as DecodeIns and also the renamed register names and
// any available values.
class ROBEntry extends DecodeIns {
  // Renamed registers (use register 0 if reg is in arch regfile)
  val rs1Rename = UInt(OUTPUT, 6)
  val rs2Rename = UInt(OUTPUT, 6)

  // Available operands (with valid bits)
  val rs1Val = Valid(UInt(OUTPUT, 32))
  val rs2Val = Valid(UInt(OUTPUT, 32))
}

class ROB extends Module {
  val io = new Bundle {
    // TODO
  }

  val rob = Vec.fill(64) { new ROBEntry() }
}

class ROBTests(c: ROB) extends Tester(c) {
  println("TODO")
}

class ROBGenerator extends TestGenerator {
  def genMod(): Module = Module(new ROB())
  def genTest[T <: Module](c: T): Tester[T] =
    (new ROBTests(c.asInstanceOf[ROB])).asInstanceOf[Tester[T]]
}
