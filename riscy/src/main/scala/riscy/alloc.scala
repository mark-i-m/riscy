package riscy

import Chisel._

class AllocRemap extends Bundle {
  val reg = UInt(OUTPUT, 5)
  val idxROB = UInt(OUTPUT, 6)
}

class RiscyAlloc extends Module {
  val io = new Bundle {
    val inst = Vec.fill(4) { Valid(new DecodeIns().flip) }

    val freeROB = UInt(INPUT, 6)
    val firstROB = UInt(INPUT, 6)

    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()) }
  }


}

class RiscyAllocTests(c: RiscyAlloc) extends Tester(c) {

}

class AllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyAllocTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
}
