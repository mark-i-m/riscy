package riscy

import Chisel._

class Stall extends Module {
  val io = new Bundle {
    // ROB and Arbiter should set their StallReq flag to signal that they would
    // like previous stages to stall while they continue to run.

    // Stages that need to stall
    val robStallReq =     Bool(INPUT)
    val arbiterStallReq = Bool(INPUT)

    // Signals that a stage should stall
    val fetchStall = Bool(OUTPUT)
    val allocStall = Bool(OUTPUT)
  }

  io.fetchStall := io.allocStall
  io.allocStall := io.arbiterStallReq || io.robStallReq

  when(io.arbiterStallReq) {
    printf("ARBITER STALL")
  }

  when(io.arbiterStallReq) {
    printf("ROB STALL")
  }
}

class StallTests(c: Stall) extends Tester(c) {
  def pokeFlags(rob: Boolean, arb: Boolean) = {
    poke(c.io.robStallReq, rob)
    poke(c.io.arbiterStallReq, arb)

    step(0)
  }

  def expectFlags(fetch: Boolean, alloc: Boolean, rob: Boolean) = {
    expect(c.io.fetchStall, fetch)
    expect(c.io.allocStall, alloc)
  }

  // nobody stalls
  pokeFlags(false, false)
  expectFlags(false, false, false)

  // rob requests stall => fetch and alloc must stall
  pokeFlags(true, false)
  expectFlags(true, true, false)

  // nobody stalls
  pokeFlags(false, false)
  expectFlags(false, false, false)

  // arbiter requests stall => rob, fetch, and alloc must stall
  pokeFlags(false, true)
  expectFlags(true, true, true)

  // arbiter requests stall => rob, fetch, and alloc must stall
  pokeFlags(true, true)
  expectFlags(true, true, true)
}

class StallGenerator extends TestGenerator {
  def genMod(): Module = Module(new Stall())
  def genTest[T <: Module](c: T): Tester[T] =
    (new StallTests(c.asInstanceOf[Stall])).asInstanceOf[Tester[T]]
}
