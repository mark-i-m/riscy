package riscy

import Chisel._

// An extended version of the decoded instruction with the same
// signals as DecodeIns and also the renamed register names and
// any available values.
class ROBEntry extends DecodeIns {
  // Renamed registers (not valid if reg is in arch regfile)
  val rs1Rename = UInt(OUTPUT, 6)
  val rs2Rename = UInt(OUTPUT, 6)

  // Available operands with ready bits
  val rs1Val = Valid(UInt(OUTPUT, 32))
  val rs2Val = Valid(UInt(OUTPUT, 32))

  // Destination register with ready bit
  val rdVal = Valid(UInt(OUTPUT, 32))

  // Unique instruction tag
  val tag = UInt(OUTPUT, 32) // TODO: How large? Can we make it the ROB entry number? 

  // Speculative bit
  val spec = Bool(OUTPUT)
}

class ROB extends Module {
  val io = new Bundle {
    // TODO
  }

  // The register remap table
  // Bit 0 of each register denotes if that register is in the Arch register
  // (1) or in the ROB (0).
  //
  // The remaining bits denote which ROB entry if the register is in the ROB
  val remap = new RegFile(32, 32, 4, i => Valid(UInt(width = 6)))

  // The ROB storage structure
  val rob = Vec.fill(64) { new ROBEntry() }

  // TODO: should we put the Arch reg file here too?
}

class ROBTests(c: ROB) extends Tester(c) {
  println("TODO")
}

class ROBGenerator extends TestGenerator {
  def genMod(): Module = Module(new ROB())
  def genTest[T <: Module](c: T): Tester[T] =
    (new ROBTests(c.asInstanceOf[ROB])).asInstanceOf[Tester[T]]
}
