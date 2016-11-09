package riscy

import Chisel._

// Based on Counter in ChiselUtils.scala
class CounterUpDown(val n: Int) {
  /** current value of the counter */
  val value = if (n == 1) UInt(0) else Reg(init=UInt(0, log2Up(n)))
  /** increment the counter
    * @return if the counter is at the max value */
   def inc(x: Int): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value > UInt(n - x - 1)
      value := Mux(wrap, UInt(0), value + UInt(x))
      wrap
    }
  }
  def inc(x: UInt): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value > (UInt(n - 1) - UInt(x))
      value := Mux(wrap, UInt(0), value + x)
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
}

class CounterUpDownModule extends Module {
  val io = new Bundle {
    val condUp = Bool(INPUT)
    val condDown = Bool(INPUT)
    val out = UInt(OUTPUT, 4)
    val wrap = Bool(OUTPUT)
  }

  val counter = new CounterUpDown(16)
  
  
  when (io.condUp && !io.condDown) {
    io.wrap := counter.inc(2)
  } .elsewhen (!io.condUp && io.condDown) {
    io.wrap := counter.dec(2)
  } .otherwise {
    io.wrap := Bool(false)
  }
 
  io.out := counter.value
}

class CounterUpDownModuleTests(c: CounterUpDownModule) extends Tester(c) {
  poke(c.io.condUp, true)
  poke(c.io.condDown, false)

  step(1)

  expect(c.io.out, 2)
  expect(c.io.wrap, false)

  poke(c.io.condUp, false)
  poke(c.io.condDown, false)

  step(1)

  expect(c.io.out, 2)

  poke(c.io.condUp, false)
  poke(c.io.condDown, true)
 
  step(1)

  expect(c.io.out, 0)
  expect(c.io.wrap, true)
  
  poke(c.io.condUp, false)
  poke(c.io.condDown, false)

  step(1)

  expect(c.io.out, 0)

  poke(c.io.condUp, false)
  poke(c.io.condDown, true)

  step(1)

  expect(c.io.out, 0)

  poke(c.io.condUp, false)
  poke(c.io.condDown, true)

  step(1)

  expect(c.io.out, 0)

  poke(c.io.condUp, true)
  poke(c.io.condDown, false)

  step(1)

  expect(c.io.out, 2)

}

class CounterUpDownModuleGenerator extends TestGenerator {
  def genMod(): Module = Module(new CounterUpDownModule)
  def genTest[T <: Module](c: T): Tester[T] =
    (new CounterUpDownModuleTests(c.asInstanceOf[CounterUpDownModule])).asInstanceOf[Tester[T]]
}
