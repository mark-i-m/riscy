package riscy

import Chisel._

class DecodeIns extends Bundle {
  val op = UInt(OUTPUT, 7)
  val rs1 = UInt(OUTPUT, 5)
  val rs2 = UInt(OUTPUT, 5)
  val rd = UInt(OUTPUT, 5)
  val funct3 = UInt(OUTPUT, 3)
  val funct7 = UInt(OUTPUT, 7)
  val immI = SInt(OUTPUT, 32)
  val immS = SInt(OUTPUT, 32)
  val immSB = SInt(OUTPUT, 32) // TODO: hook this up
  val immB = SInt(OUTPUT, 32)
  val immU = SInt(OUTPUT, 32)
  val immJ = SInt(OUTPUT, 32)
  val immUJ = SInt(OUTPUT, 32)  // TODO: hook this up
}

class RiscyDecode extends Module {
  val io = new Bundle {
    // Input from I$
    val ins = UInt(INPUT, 32)

    // Decoded output signals
    var decoded = new DecodeIns()
  }

  // Split up wires from the instruction
  io.decoded.op     := io.ins(6, 0)
  io.decoded.rd     := io.ins(11, 7)
  io.decoded.rs1    := io.ins(19, 15)
  io.decoded.rs2    := io.ins(24, 20)
  io.decoded.funct3 := io.ins(14, 12)
  io.decoded.funct7 := io.ins(31, 25)

  // Construct the 5 types of immediates and let other
  // modules choose what they want.
  io.decoded.immI   := Cat(Fill(20, io.ins(31)), io.ins(31, 20))
  io.decoded.immS   := Cat(Fill(20, io.ins(31)), io.ins(31, 25), io.ins(11, 7))
  io.decoded.immB   := Cat(Fill(19, io.ins(31)), io.ins(31), io.ins(7), io.ins(30, 25), io.ins(11, 8), UInt(0, 1))
  io.decoded.immU   := Cat(io.ins(31, 12), UInt(0, 12))
  io.decoded.immJ   := Cat(Fill(11, io.ins(31)), io.ins(31), io.ins(19, 12), io.ins(20), io.ins(30, 25), io.ins(24, 21), UInt(0, 1))
}

class RiscyDecodeTests(c: RiscyDecode) extends Tester(c) {
  // Try decoding an instruction
  poke(c.io.ins, 0xAAAAAAAA)

  step(1)

  expect(c.io.decoded.op, 0x2A)
  expect(c.io.decoded.rs1, 0x15)
  expect(c.io.decoded.rs2, 0x0A)
  expect(c.io.decoded.rd, 0x15)
  expect(c.io.decoded.funct3, 0x2)
  expect(c.io.decoded.funct7, 0x55)
  expect(c.io.decoded.immI, 0xFFFFFAAA)
  expect(c.io.decoded.immS, 0xFFFFFAB5)
  expect(c.io.decoded.immB, 0xFFFFFAB4)
  expect(c.io.decoded.immU, 0xAAAAA000)
  expect(c.io.decoded.immJ, 0xFFFAA2AA)
}

class DecodeGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyDecode())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyDecodeTests(c.asInstanceOf[RiscyDecode])).asInstanceOf[Tester[T]]
}
