package riscy

import Chisel._

/**
 * 4-wide instruction fetch block for RISCY
 */

// The output signals for Decode stage are:
// 1. Four 32b instructions: Fetch.icache.io.resp.inst(i)
// 2. Four instruction valid bits: Fetch.io.instValid(i)
//
class Fetch extends Module {
  val io = new Bundle {
    /* INPUTS */
    val btbAddr = UInt(INPUT, 32)
    val rasAddr = UInt(INPUT, 32)
    val isBranchTaken = Bool(INPUT)
    val branchMispredAddr = UInt(INPUT, 32)
    val isBranchMispred = Bool(INPUT)
    // Is this instruction a return from a subroutine call?
    val isReturn = Bool(INPUT)
    val stall = Bool(INPUT)

    /* OUTPUTS */
    // Instructions to be passed to the decode stage
    val insts = Vec.fill(4) { UInt(OUTPUT, 32) }
    // Are the instructions to be passed over to the decode valid?
    val instValid = Vec.fill(4) { Bool(OUTPUT) }
    // PC to be passed down the pipeline
    val fetchBlockPC = UInt(OUTPUT, 32)
  }
  val icache = Module(new ICache())

  /* PC value takes a cycle to reach Icache. We start it at 0x10 so that we
   * don't lose the first cycle. The pipeline register which PC feeds starts at
   * 0x0 */
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

  /* Register which holds the address to be sent to Icache. PC value appears here
   * after one cycle delay */
  val fetchAddr = Reg(init = UInt(0, width = 32))
  // Used to correct requested PC incase Icache tells us that it is not ready
  val prevFetchAddr = Reg(init = UInt(0, width = 32))
  val icache_ready = icache.io.resp.idle || icache.io.resp.valid

  // Shift in a new address only is the cache is ready to accept the old address
  when (icache_ready) {
    fetchAddr := addr
    prevFetchAddr := fetchAddr
  }

  // Send out the address to Icache.
  // NOTE - We need to reuse the previously issued request if Icache is not
  // ready. This is because Icache only keeps a hold of the addr which
  // generated a miss 2 cycles ago and drops the one which was issued 1 cycle
  // ago.
  icache.io.req.addr := Mux(icache_ready, fetchAddr, prevFetchAddr)
  icache.io.req.valid := Bool(true)

  val nextPC = UInt(width = 32)
  val nextPCOffset = UInt(width = 5)
  nextPCOffset := UInt(16)

  // Calculate next sequential fetch address. Don't increment if stalled
  nextPC := addr + Mux(io.stall, UInt(0), nextPCOffset)

  // Increment PC
  when ((io.stall === Bool(false)) && icache_ready) {
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
  when (icache.io.resp.valid && (addr(4,0) === UInt(20))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(true)
    io.instValid(2) := Bool(true)
    io.instValid(3) := Bool(false)
  } .elsewhen (icache.io.resp.valid && (addr(4,0) === UInt(24))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(true)
    io.instValid(2) := Bool(false)
    io.instValid(3) := Bool(false)
  } .elsewhen (icache.io.resp.valid && (addr(4,0) === UInt(28))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(false)
    io.instValid(2) := Bool(false)
    io.instValid(3) := Bool(false)
  } .elsewhen (icache.io.resp.valid) {
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
  io.fetchBlockPC := icache.io.resp.addr

  for (i <- 0 until 4) {
    io.insts(i) := icache.io.resp.inst(i)
  }
}

class FetchTests(c: Fetch) extends Tester(c) { 
  // Utility functions
  def expect_all_inst_validity(c : Fetch, value : Boolean) = {
    expect(c.io.instValid(0), value)
    expect(c.io.instValid(1), value)
    expect(c.io.instValid(2), value)
    expect(c.io.instValid(3), value)
  }
  def peek_regs(c : Fetch) = {
    // Peek important registers. Feel free to comment out anything unimportant
    // to you
    peek(c.PC)
    peek(c.fetchAddr)
    peek(c.prevFetchAddr)
  }

  // Setup necessary stuff for testcase
  poke(c.io.btbAddr, 0xbbbbbbbb)
  poke(c.io.rasAddr, 0xdddddddd)
  poke(c.io.isBranchTaken, false)
  poke(c.io.branchMispredAddr, 0xeeeeeeee)
  poke(c.io.isBranchMispred, false)
  poke(c.io.isReturn, false)
  poke(c.io.stall, false)

  // We expect the Icache to be ready for taking responses ie. idle should be
  // true. 
  expect(c.icache.io.resp.valid, false)
  expect(c.icache.io.resp.idle, true)
  expect(c.icache.io.req.addr, 0x0)
  expect_all_inst_validity(c, false)
  // It will take us 5 cycles to load the first block into the cache
  step(1)

  // Cycle 1 - Icache has taken in 0x0. It can take in another request
  expect(c.icache.io.resp.valid, false)
  expect(c.icache.io.resp.idle, true)
  expect(c.icache.io.req.addr, 0x10)
  expect_all_inst_validity(c, false)
  peek_regs(c)
  step(1)

  // Cycle 2 - Icache will have seen a miss. It will now proceed to refill.
  expect(c.icache.io.resp.valid, false)
  expect(c.icache.io.resp.idle, false)
  expect(c.icache.io.req.addr, 0x10)
  expect_all_inst_validity(c, false)
  peek_regs(c)
  step(1)

  // Cycle 3 - Icache busy refilling
  expect(c.icache.io.resp.valid, false)
  expect(c.icache.io.resp.idle, false)
  expect(c.icache.io.req.addr, 0x10)
  expect_all_inst_validity(c, false)
  peek_regs(c)
  step(1)

  // Cycle 4 - Icache busy refilling
  expect(c.icache.io.resp.valid, false)
  expect(c.icache.io.resp.idle, false)
  expect(c.icache.io.req.addr, 0x10)
  expect_all_inst_validity(c, false)
  peek_regs(c)
  step(1)

  // Cycle 5 - We should have gotten a hit now for 0x0
  expect(c.icache.io.resp.valid, true)
  expect(c.icache.io.resp.idle, true)
  expect(c.icache.io.resp.addr, 0x0)
  expect(c.icache.io.req.addr, 0x20)
  expect_all_inst_validity(c, true)
  peek_regs(c)
  step(1)

  // Cycle 6 - Another hit
  expect(c.icache.io.resp.valid, true)
  expect(c.icache.io.resp.idle, true)
  expect(c.icache.io.resp.addr, 0x10)
  expect(c.icache.io.req.addr, 0x30)
  expect_all_inst_validity(c, true)
  peek_regs(c)
  step(1)

  // Cycle 7 - This should be a miss since the 0x20 requested 2 cycles ago
  // doesn't exist in cache
  expect(c.icache.io.resp.valid, false)
  expect(c.icache.io.resp.idle, false)
  expect(c.icache.io.req.addr, 0x30)
  expect_all_inst_validity(c, false)
  peek_regs(c)
  step(1)

  // Cycle 8 - Icache busy filling response
  expect(c.icache.io.resp.valid, false)
  expect(c.icache.io.resp.idle, false)
  expect(c.icache.io.req.addr, 0x30)
  expect_all_inst_validity(c, false)
  peek_regs(c)
} 

class FetchGenerator extends TestGenerator {
  def genMod(): Module = Module(new Fetch())
  def genTest[T <: Module](c: T): Tester[T] = 
    (new FetchTests(c.asInstanceOf[Fetch])).asInstanceOf[Tester[T]]
}
