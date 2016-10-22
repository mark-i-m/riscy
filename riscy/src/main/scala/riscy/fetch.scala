package riscy

import Chisel._

/**
 * Implementation of fetch stage
 */

class Fetch extends Module {
  val io = new Bundle {
    val btbAddr = UInt(INPUT, 32)
    val rasAddr = UInt(INPUT, 32)
    val isBranch = Bool(INPUT)
    val branchMispredAddr = UInt(INPUT, 32)
    val isBranchMispred = Bool(INPUT)
    val isReturn = Bool(INPUT)
    val stall = Bool(INPUT)
    val icacheRespAddr = UInt(INPUT, 32)
    val icacheRespValues = Vec.fill(4) { UInt(INPUT, 32) }
    val hit_notmiss = Bool(INPUT)
    val dbgin = UInt(INPUT, 1)

    val dbgout = UInt(OUTPUT, 1)
    val icacheAddr = UInt(OUTPUT, 32)
    val insts = Vec.fill(4) { UInt(OUTPUT, 32) }
    val instValid = Vec.fill(4) { Bool(OUTPUT) }
    val fetchBlockPC = UInt(OUTPUT, 32)
  }

  val PC = Reg(init = UInt(0, width = 32)) 

  val nextAddr = Mux(io.isBranch, io.btbAddr, PC)

  val addr = UInt(width = 32)
  val addrSelect = UInt(width = 2)
  addrSelect := Cat(io.isBranchMispred, io.isReturn).toBits().toUInt()
  addr := nextAddr

  when (addrSelect === UInt(0)) {
    addr := nextAddr
  } .elsewhen (addrSelect === UInt(1)) {
    addr := io.rasAddr
  } .elsewhen (addrSelect === UInt(2)) {
    addr := io.branchMispredAddr
  } .elsewhen (addrSelect === UInt(3)) {
    addr := io.branchMispredAddr
  }

  val fetchAddr = Reg(next = addr)
  io.icacheAddr := fetchAddr

  val nextPC = UInt(width = 32)
  val nextPCOffset = UInt(width = 5)
  nextPCOffset := UInt(16)
  nextPC := io.icacheAddr + Mux(io.stall, UInt(0), nextPCOffset)

  unless (io.stall) { PC := nextPC }

  when (io.hit_notmiss && (io.icacheRespAddr(4,0) === UInt(29))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(true)
    io.instValid(2) := Bool(true)
    io.instValid(3) := Bool(false)
    nextPCOffset := UInt(12)
  } .elsewhen (io.hit_notmiss && (io.icacheRespAddr(4,0) === UInt(30))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(true)
    io.instValid(2) := Bool(false)
    io.instValid(3) := Bool(false)
    nextPCOffset := UInt(8)
  } .elsewhen (io.hit_notmiss && (io.icacheRespAddr(4,0) === UInt(31))) {
    io.instValid(0) := Bool(true)
    io.instValid(1) := Bool(false)
    io.instValid(2) := Bool(false)
    io.instValid(3) := Bool(false)
    nextPCOffset := UInt(4)
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

  io.fetchBlockPC := io.icacheRespAddr

  for (i <- 0 until 4) {
    io.insts(i) := io.icacheRespValues(i)
  }

  io.dbgout := io.dbgin
}

class FetchTests(c: Fetch) extends Tester(c) { 
  poke(c.io.dbgin, 1)
  step(1) 
  expect(c.io.dbgout, 1) 
  poke(c.io.dbgin, 0)
  step(1) 
  expect(c.io.dbgout, 0) 
  poke(c.io.dbgin, 1)
  step(1) 
  expect(c.io.dbgout, 1) 
} 

class FetchGenerator extends TestGenerator {
  def genMod(): Module = Module(new Fetch())
  def genTest[T <: Module](c: T): Tester[T] = 
    (new FetchTests(c.asInstanceOf[Fetch])).asInstanceOf[Tester[T]]
}
