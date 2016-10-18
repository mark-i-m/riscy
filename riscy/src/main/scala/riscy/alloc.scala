package riscy

import Chisel._

// Information from the Allocate/Rename stage to the Remap table.
class AllocRemap extends Bundle {
  val reg = UInt(OUTPUT, 5) // which reg to rename
  val idxROB = UInt(OUTPUT, 6) // ROB entry number to map to
}

// Information from the Allocate/Rename stage to the ROB and IQs.
class AllocROB extends Bundle {
  val entry = UInt(OUTPUT, 6) // which ROB entry to latch
  val ins = new ROBEntry() // the renamed instruction
}

// The Control Magic to rename instructions and send signals to update the
// Remap table, the ROB, and the Issue Queues.
class RiscyAlloc extends Module {
  val io = new Bundle {
    // Input from the rotator and decode logic with 4 decoded instructions
    val inst = Vec.fill(4) { Valid(new DecodeIns()).flip }

    // Inputs with the first free entry and the number of free entries in the ROB
    val freeROB = UInt(INPUT, 6)
    val firstROB = UInt(INPUT, 6)

    // Input from the Remap table to find out what the current mappings are (so
    // we can rename)
    // TODO

    // Outputs to the Remap table and the ROB with the correct values to update
    // for this cycle. Note that they might not all be valid.
    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()) }
    val allocROB = Vec.fill(4) { Valid(new AllocROB()) }
  }

  // Do a simple addition to rename the instructions. Every instruction gets
  // an ROB entry, regardless of how many registers it reads or writes. The
  // valid bits of the instructions and the number of free ROB entries determines
  // whether we should stall and how many entries we should put into the ROB.
  val renamedDest = Vec.tabulate(4) { i => io.freeROB + UInt(i, 2) }
}

class RiscyAllocTests(c: RiscyAlloc) extends Tester(c) {
  // Try some simple renaming then get more complex.
  // TODO: add more tests
  
  // add r1 <- r1 + 0xFFF
  poke(c.io.inst(0).valid, 1)
  poke(c.io.inst(0).bits.op, 0x33)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x1)
  poke(c.io.inst(0).bits.rd, 0x1)
  poke(c.io.inst(0).bits.immI, 0xFFF)

  poke(c.io.freeROB, 0)
  poke(c.io.firstROB, 0)

  step(1)

  // Should map r1 to ROB0
  expect(c.io.allocRemap(0).valid, 1)
  expect(c.io.allocRemap(0).bits.reg, 1)
  expect(c.io.allocRemap(0).bits.idxROB, 0)


}

class AllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyAllocTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
}
