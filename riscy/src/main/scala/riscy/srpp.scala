package riscy

import Chisel._

class ShiftRegPP[T <: Data](gen: () => T) extends Module {
	val io = new Bundle {
		val wen = Bool(INPUT)
		val data = gen().asInput
		val value = gen().asOutput
	}

	val data = Reg(outType = gen())
	
	when(io.wen) {
		data := io.data
	} .otherwise {
		data := data
	}

	io.value := data
}

class ShiftRegPPTests(c: ShiftRegPP[UInt]) extends Tester(c) {
	poke(c.io.wen, 1)
	poke(c.io.data, 0x12341234)

	step(1)

	expect(c.io.value, 0x12341234)

	poke(c.io.wen, 0)
	poke(c.io.data, 0x5)

	step(1)

	expect(c.io.value, 0x12341234)

	poke(c.io.wen, 1)
	poke(c.io.data, 0x20160323)

	step(1)

	expect(c.io.value, 0x20160323)
}

class ShiftRegPPGenerator extends TestGenerator {
  def genMod(): Module = Module(new ShiftRegPP(() => UInt(width = 64)))
  def genTest[T <: Module](c: T): Tester[T] =
    (new ShiftRegPPTests(c.asInstanceOf[ShiftRegPP[UInt]])).asInstanceOf[Tester[T]]
}
