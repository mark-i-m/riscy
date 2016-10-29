package riscy

import Chisel._

/**
 * 4-wide instruction fetch block for RISCY
 */

class Fetch extends Module {
  val io = new Bundle {
    /* INPUTS */
    val btbAddr = UInt(INPUT, 32)

    val rasAddr = UInt(INPUT, 32)

    val isBranchTaken = Bool(INPUT)

    val branchMispredAddr = UInt(INPUT, 32)

    val isBranchMispred = Bool(INPUT)

    val isReturn = Bool(INPUT)              // Is this instruction a return
                                            // from a subroutine call?

    val stall = Bool(INPUT)

    val icacheRespAddr = UInt(INPUT, 32)    // Address corresponding to the
                                            // instructions received from I$

    val icacheRespValues = Vec.fill(4) { UInt(INPUT, 32) }

    val hit_notmiss = Bool(INPUT)           // Did we get a hit in ICache?

    val icacheReady = Bool(INPUT)           // Is the ICache ready to fetch
                                            // at a new address?

    /* OUTPUTS */
    val icacheReqAddr = UInt(OUTPUT, 32)    // What address should we request
                                            // the ICache to fetch from?

    val insts = Vec.fill(4) { UInt(OUTPUT, 32) }  // Instructions to be passed
                                                  // to decode stage

    val instValid = Vec.fill(4) { Bool(OUTPUT) }  // Are the respective
                                                  // instructions valid?

    val fetchBlockPC = UInt(OUTPUT, 32)           // Program Counter to be
                                                  // passed down the pipeline
  }

  /* PC value takes a cycle to reach I$. We start it at 0x10 so that we don't lose the
   * first cycle. The pipeline register which PC feeds starts at 0x0 */
  val PC = Reg(init = UInt(16, width = 32))

  val nextAddr = Mux(io.isBranchTaken, io.btbAddr, PC)

  val addr = UInt(width = 32)
  val addrSelect = UInt(width = 2)
  addrSelect := Cat(io.isBranchMispred, io.isReturn).toBits().toUInt()
  addr := nextAddr

  /* 4-way multiplexer to choose fetch address */
  when (addrSelect === UInt(0)) {
    addr := nextAddr
  } .elsewhen (addrSelect === UInt(1)) {
    addr := io.rasAddr
  } .elsewhen (addrSelect === UInt(2)) {
    addr := io.branchMispredAddr
  } .elsewhen (addrSelect === UInt(3)) {
    addr := io.branchMispredAddr
  }

  /* Register which holds the address to be sent to I$. PC value appears here
   * after one cycle delay */
  val fetchAddr = Reg(init = UInt(0, width = 32))

  // Shift in a new address only is the cache is ready to accept the old address
  when (io.icacheReady) {
    fetchAddr := addr
  } .otherwise {
    fetchAddr := fetchAddr
  }

  // Send out the address to I$
  io.icacheReqAddr := fetchAddr

  val nextPC = UInt(width = 32)
  val nextPCOffset = UInt(width = 5)
  nextPCOffset := UInt(16)

  // Calculate next sequential fetch address. Don't increment if stalled
  nextPC := addr + Mux(io.stall, UInt(0), nextPCOffset)

  // Increment PC
  when ((io.stall === Bool(false)) && io.icacheReady) {
    PC := nextPC
  }

  /* Assuming a 32B cache block, calculate the next sequential fetch address
   * when we get close to a cache line boundary */
  when (addr(4, 0) === UInt(20)) {
    nextPCOffset := UInt(12)
  } .elsewhen (addr(4,0) === UInt(24)) {
    nextPCOffset := UInt(8)
  } .elsewhen (addr(4, 0) === UInt(28)) {
    nextPCOffset := UInt(4)
  } .otherwise {
    nextPCOffset := UInt(16)
  }

  /* Figure out which instructions are valid if we fetched close to a cache
   * line boundary */
  when (io.hit_notmiss && (addr(4,0) === UInt(20))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(true)
    io.instValid(2) := Bool(true)
    io.instValid(3) := Bool(false)
  } .elsewhen (io.hit_notmiss && (addr(4,0) === UInt(24))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(true)
    io.instValid(2) := Bool(false)
    io.instValid(3) := Bool(false)
  } .elsewhen (io.hit_notmiss && (addr(4,0) === UInt(28))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(false)
    io.instValid(2) := Bool(false)
    io.instValid(3) := Bool(false)
  } .elsewhen (io.hit_notmiss) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(true)
    io.instValid(2) := Bool(true)
    io.instValid(3) := Bool(true)
  } .otherwise {
    io.instValid(0) := Bool(false)
    io.instValid(1) := Bool(false)
    io.instValid(2) := Bool(false)
    io.instValid(3) := Bool(false)
  }

  // Pass the PC value down the pipeline
  io.fetchBlockPC := io.icacheRespAddr

  for (i <- 0 until 4) {
    io.insts(i) := io.icacheRespValues(i)
  }
}

class FetchTests(c: Fetch) extends Tester(c) { 
  poke(c.io.btbAddr, 0xbbbbbbbb)
  poke(c.io.rasAddr, 0xdddddddd)
  poke(c.io.isBranchTaken, false)
  poke(c.io.branchMispredAddr, 0xeeeeeeee)
  poke(c.io.isBranchMispred, false)
  poke(c.io.isReturn, false)
  poke(c.io.stall, false)
  poke(c.io.icacheRespAddr, 0xcccccccc)
  poke(c.io.icacheRespValues(0), 0x11111111)
  poke(c.io.icacheRespValues(1), 0x22222222)
  poke(c.io.icacheRespValues(2), 0x33333333)
  poke(c.io.icacheRespValues(3), 0x44444444)
  poke(c.io.hit_notmiss, 0)
  poke(c.io.icacheReady, 1)
  expect(c.io.icacheReqAddr, 0x0)
  expect(c.io.instValid(0), false)
  expect(c.io.instValid(1), false)
  expect(c.io.instValid(2), false)
  expect(c.io.instValid(3), false)
  step(1)

  poke(c.io.icacheReady, 0)
  expect(c.io.icacheReqAddr, 0x10)
  expect(c.io.instValid(0), false)
  expect(c.io.instValid(1), false)
  expect(c.io.instValid(2), false)
  expect(c.io.instValid(3), false)
  step(1)

  expect(c.io.icacheReqAddr, 0x10)
  expect(c.io.instValid(0), false)
  expect(c.io.instValid(1), false)
  expect(c.io.instValid(2), false)
  expect(c.io.instValid(3), false)
  poke(c.io.hit_notmiss, 1)
  poke(c.io.icacheReady, 1)
  poke(c.io.icacheRespAddr, 0x0)
  step(1)

  poke(c.io.icacheReady, 1)
  expect(c.io.icacheReqAddr, 0x20)
  expect(c.io.instValid(0), true)
  expect(c.io.instValid(1), true)
  expect(c.io.instValid(2), true)
  expect(c.io.instValid(3), true)
  step(1)

  expect(c.io.icacheReqAddr, 0x30)
  poke(c.io.icacheReady, 0)
  poke(c.io.hit_notmiss, 1)
  poke(c.io.icacheRespAddr, 0x10)
  step(1)

  expect(c.io.icacheReqAddr, 0x30)
  poke(c.io.hit_notmiss, 0)
  poke(c.io.icacheReady, 0)
  poke(c.io.icacheRespAddr, 0x20)
  step(1)

  expect(c.io.icacheReqAddr, 0x30)
  poke(c.io.hit_notmiss, 1)
  poke(c.io.icacheReady, 1)
  poke(c.io.icacheRespAddr, 0x20)
  step(1)

  expect(c.io.icacheReqAddr, 0x40)
  poke(c.io.hit_notmiss, 1)
  poke(c.io.icacheReady, 1)
  poke(c.io.icacheRespAddr, 0x30)
  step(1)

  expect(c.io.icacheReqAddr, 0x50)
} 

class FetchGenerator extends TestGenerator {
  def genMod(): Module = Module(new Fetch())
  def genTest[T <: Module](c: T): Tester[T] = 
    (new FetchTests(c.asInstanceOf[Fetch])).asInstanceOf[Tester[T]]
}
