
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

  io.op     := ins(6, 0)
  io.rd     := ins(11, 7)
  io.rs1    := ins(19, 15)
  io.rs2    := ins(24, 20)
  io.funct3 := ins(14, 12)
  io.funct7 := ins(31, 25)

  io.immI   := ins(31, 20)
  io.immS   := Cat(ins(31, 25), ins(11, 7))
  io.immB   := Cat(ins(31), ins(7), ins(30, 25), ins(11, 8))
  io.immU   := Cat(ins(31), ins(30, 20), ins(19, 12))
  io.immJ   := Cat(ins(31), ins(19, 12), ins(20), ins(30, 25), ins(24, 21))
}
