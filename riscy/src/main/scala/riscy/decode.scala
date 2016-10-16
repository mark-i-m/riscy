package riscy

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

  io.immI   := Cat(Fill(20, io.ins(31)), io.ins(31, 20))
  io.immS   := Cat(Fill(20, io.ins(31)), io.ins(31, 25), io.ins(11, 7))
  io.immB   := Cat(Fill(19, io.ins(31)), io.ins(31), io.ins(7), io.ins(30, 25), io.ins(11, 8), UInt(0, 1))
  io.immU   := Cat(io.ins(31, 12), UInt(0, 12))
  io.immJ   := Cat(Fill(11, io.ins(31)), io.ins(31), io.ins(19, 12), io.ins(20), io.ins(30, 25), io.ins(24, 21), UInt(0, 1))
}

class RiscyDecodeTests(c: RiscyDecode) extends Tester(c) {
  poke(c.io.ins, 0xAAAAAAAA)

  step(1)

  expect(c.io.op, 0x2A) // DONE
  expect(c.io.rs1, 0x15) // DONE
  expect(c.io.rs2, 0x0A) // DONE
  expect(c.io.rd, 0x15) // DONE
  expect(c.io.funct3, 0x2) // DONE
  expect(c.io.funct7, 0x55) // DONE
  expect(c.io.immI, 0xFFFFFAAA) // DONE
  expect(c.io.immS, 0xFFFFFAB5) // DONE
  expect(c.io.immB, 0xFFFFFAB4) // DONE
  expect(c.io.immU, 0xAAAAA000) // DONE
  expect(c.io.immJ, 0xFFFAA2AA) // DONE
}

class DecodeGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyDecode())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyDecodeTests(c.asInstanceOf[RiscyDecode])).asInstanceOf[Tester[T]]
}
