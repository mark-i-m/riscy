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
  def inc(x: UInt): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value > (UInt(n - 1) - UInt(x))
      value := Mux(Bool(!isPow2(n)) && wrap, UInt(0), value + x)
      wrap
    }
  }

  def dec(x: Int): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value < UInt(x)
      value := Mux(wrap, UInt(0), value - UInt(x))
      wrap
    }
  }
  def dec(x: UInt): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value < x
      value := Mux(wrap, UInt(0), value - x)
      wrap
    }
  }

  def reset() = value := UInt(0)
}

class MultiCounterTestModule extends Module {
  val io = new Bundle {
    val inc1 = Bool(INPUT)
    val inc2 = Bool(INPUT)
    val inc3 = Bool(INPUT)
    val inc4 = Bool(INPUT)
    val inc5 = Bool(INPUT)
    val out = UInt(OUTPUT, 4)
  }

  val counter = new MultiCounter(16)

  when (io.inc1) {
    counter inc 2 
  } .elsewhen (io.inc2) {
    counter inc UInt(4) 
  } .elsewhen (io.inc3) {
    counter reset
  } .elsewhen (io.inc4) {
    counter dec UInt(2)
  } .elsewhen (io.inc5) {
    counter dec 4
  }

  io.out := counter.value
}

class MultiCounterTests(c: MultiCounterTestModule) extends Tester(c) {
  poke(c.io.inc1, true)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 6)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 6)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 10)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 14)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, true)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 0)

  poke(c.io.inc1, true)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 6)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 6)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 10)

  poke(c.io.inc1, false)
  poke(c.io.inc2, true)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 14)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, true)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 0)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, true)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 0)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, false)
  poke(c.io.inc4, false)
  poke(c.io.inc5, true)

  step(1)

  expect(c.io.out, 0)

  poke(c.io.inc1, false)
  poke(c.io.inc2, false)
  poke(c.io.inc3, true)
  poke(c.io.inc4, false)
  poke(c.io.inc5, false)

  step(1)

  expect(c.io.out, 0)
}

class MultiCounterGenerator extends TestGenerator {
  def genMod(): Module = Module(new MultiCounterTestModule)
  def genTest[T <: Module](c: T): Tester[T] =
    (new MultiCounterTests(c.asInstanceOf[MultiCounterTestModule])).asInstanceOf[Tester[T]]
}
