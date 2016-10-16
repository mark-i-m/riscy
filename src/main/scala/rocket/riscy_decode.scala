
package rocket

import Chisel._

class RiscyDecode extends Module {
  val io = new Bundle {
    // Input from I$
    val ins = UInt(INPUT, 32);

    // Decoded output signals
    val op = UInt(OUTPUT, 7)
    val rs1 = UInt(OUTPUT, 5)
    val rs2 = UInt(OUTPUT, 5)
    val rd = UInt(OUTPUT, 5)
    val funct3 = UInt(OUTPUT, 3)
    val funct7 = UInt(OUTPUT, 7)
    val immI = SInt(OUTPUT, 32)
    val immS = SInt(OUTPUT, 32)
    val immB = SInt(OUTPUT, 32)
    val immU = SInt(OUTPUT, 32)
    val immJ = SInt(OUTPUT, 32)
  }

  io.op     := io.ins(6, 0)
  io.rd     := io.ins(11, 7)
  io.rs1    := io.ins(19, 15)
  io.rs2    := io.ins(24, 20)
  io.funct3 := io.ins(14, 12)
  io.funct7 := io.ins(31, 25)

  io.immI   := io.ins(31, 20)
  io.immS   := Cat(io.ins(31, 25), io.ins(11, 7))
  io.immB   := Cat(io.ins(31), io.ins(7), io.ins(30, 25), io.ins(11, 8))
  io.immU   := Cat(io.ins(31), io.ins(30, 20), io.ins(19, 12))
  io.immJ   := Cat(io.ins(31), io.ins(19, 12), io.ins(20), io.ins(30, 25), io.ins(24, 21))
}

class RiscyDecodeTests(c: RiscyDecode) extends Tester(c) {

}
