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

class RiscyAlloc extends Module {
  val io = new Bundle {
    val inst = Vec.fill(4) { Valid(new DecodeIns().flip) }

    val freeROB = UInt(INPUT, 6)
    val firstROB = UInt(INPUT, 6)

    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()) }
    val allocROB = Vec.fill(4) { Valid(new AllocROB()) }
  }
}

class RiscyAllocTests(c: RiscyAlloc) extends Tester(c) {
  println("TODO")
}

class AllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyAllocTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
}
