package riscy

import Chisel._

// Based on Counter in ChiselUtils.scala
class MultiCounter(val n: Int) {
  /** current value of the counter */
  val value = if (n == 1) UInt(0) else Reg(init=UInt(0, log2Up(n)))
  /** increment the counter
    * @return if the counter is at the max value */
  def inc(x: Int): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value > UInt(n - x - 1)
      value := Mux(Bool(!isPow2(n)) && wrap, UInt(0), value + UInt(x))
      wrap
    }
  }
}

class MultiCounterTestModule extends Module {
  val io = new Bundle {
    val inc1 = Bool(INPUT)
    val inc2 = Bool(INPUT)
    val out = UInt(OUTPUT, 4)
  }

  val counter = new MultiCounter(16)

  when (io.inc1) {
    counter.inc(2);
  } .elsewhen (io.inc2) {
    counter.inc(4);
  }

  io.out := counter.value
}

class MultiCounterTests(c: MultiCounterTestModule) extends Tester(c) {
  poke(c.io.inc1, true)
  poke(c.io.inc2, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)

  step(1)

  expect(c.io.out, 6)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)

  step(1)

  expect(c.io.out, 6)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)

  step(1)

  expect(c.io.out, 10)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)

  step(1)

  expect(c.io.out, 14)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)

  step(1)

  expect(c.io.out, 2)
}

class MultiCounterGenerator extends TestGenerator {
  def genMod(): Module = Module(new MultiCounterTestModule)
  def genTest[T <: Module](c: T): Tester[T] =
    (new MultiCounterTests(c.asInstanceOf[MultiCounterTestModule])).asInstanceOf[Tester[T]]
}
