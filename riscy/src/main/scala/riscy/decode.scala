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
  val immB = SInt(OUTPUT, 32)
  val immU = SInt(OUTPUT, 32)
  val immJ = SInt(OUTPUT, 32)
}

class RiscyDecodeSingle extends Module {
  val io = new Bundle {
    // Input from I$
    val ins = Valid(UInt(INPUT, 32)).asInput
		val pc = Vec.fill(4) {UInt(INPUT, 64)}
    // Decoded output signals
    var decoded = Valid(new DecodeIns())
  }
  
	// Passing on valid signal 
	io.decoded.valid 			 := io.ins.valid
  // Split up wires from the instruction
  io.decoded.bits.op     := io.ins.bits(6, 0)
  io.decoded.bits.rd     := io.ins.bits(11, 7)
  io.decoded.bits.rs1    := io.ins.bits(19, 15)
  io.decoded.bits.rs2    := io.ins.bits(24, 20)
  io.decoded.bits.funct3 := io.ins.bits(14, 12)
  io.decoded.bits.funct7 := io.ins.bits(31, 25)

  // Construct the 5 types of immediates and let other
  // modules choose what they want.
  io.decoded.bits.immI   := Cat(Fill(20, io.ins.bits(31)), io.ins.bits(31, 20))
  io.decoded.bits.immS   := Cat(Fill(20, io.ins.bits(31)), io.ins.bits(31, 25), io.ins.bits(11, 7))
  io.decoded.bits.immB   := Cat(Fill(19, io.ins.bits(31)), io.ins.bits(31), io.ins.bits(7), io.ins.bits(30, 25), io.ins.bits(11, 8), UInt(0, 1))
  io.decoded.bits.immU   := Cat(io.ins.bits(31, 12), UInt(0, 12))
  io.decoded.bits.immJ   := Cat(Fill(11, io.ins.bits(31)), io.ins.bits(31), io.ins.bits(19, 12), io.ins.bits(20), io.ins.bits(30, 25), io.ins.bits(24, 21), UInt(0, 1))
}

class RiscyDecodeTests(c: RiscyDecode) extends Tester(c) {
  // Try decoding an instruction
  poke(c.io.ins.valid, 1)
	poke(c.io.ins.bits, 0xAAAAAAAA)

  step(1)

  expect(c.io.decoded.bits.op, 0x2A)
  expect(c.io.decoded.bits.rs1, 0x15)
  expect(c.io.decoded.bits.rs2, 0x0A)
  expect(c.io.decoded.bits.rd, 0x15)
  expect(c.io.decoded.bits.funct3, 0x2)
  expect(c.io.decoded.bits.funct7, 0x55)
  expect(c.io.decoded.bits.immI, 0xFFFFFAAA)
  expect(c.io.decoded.bits.immS, 0xFFFFFAB5)
  expect(c.io.decoded.bits.immB, 0xFFFFFAB4)
  expect(c.io.decoded.bits.immU, 0xAAAAA000)
  expect(c.io.decoded.bits.immJ, 0xFFFAA2AA)
}

class DecodeGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyDecode())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyDecodeTests(c.asInstanceOf[RiscyDecode])).asInstanceOf[Tester[T]]
}
