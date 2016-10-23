package riscy

import Chisel._

/**
 * Implementation of fetch stage
 */

class Fetch extends Module {
  val io = new Bundle {
    val btbAddr = UInt(INPUT, 32)
    val rasAddr = UInt(INPUT, 32)
    val isBranchTaken = Bool(INPUT)
    val branchMispredAddr = UInt(INPUT, 32)
    val isBranchMispred = Bool(INPUT)
    val isReturn = Bool(INPUT)
    val stall = Bool(INPUT)
    val icacheRespAddr = UInt(INPUT, 32)
    val icacheRespValues = Vec.fill(4) { UInt(INPUT, 32) }
    val hit_notmiss = Bool(INPUT)

    val icacheReqAddr = UInt(OUTPUT, 32)
    val insts = Vec.fill(4) { UInt(OUTPUT, 32) }
    val instValid = Vec.fill(4) { Bool(OUTPUT) }
    val fetchBlockPC = UInt(OUTPUT, 32)
  }

  val PC = Reg(init = UInt(16, width = 32))

  val nextAddr = Mux(io.isBranchTaken, io.btbAddr, PC)

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

  val fetchAddr = Reg(init = UInt(0, width = 32))
  when (io.hit_notmiss) {
    fetchAddr := addr
  } .otherwise {
    fetchAddr := fetchAddr
  }

  io.icacheReqAddr := fetchAddr

  val nextPC = UInt(width = 32)
  val nextPCOffset = UInt(width = 5)
  nextPCOffset := UInt(16)
  nextPC := addr + Mux(io.stall, UInt(0), nextPCOffset)

  when ((io.stall === Bool(false)) && io.hit_notmiss) {
    PC := nextPC
  }

  when (addr(4, 0) === UInt(20)) {
    nextPCOffset := UInt(12)
  } .elsewhen (addr(4,0) === UInt(24)) {
    nextPCOffset := UInt(8)
  } .elsewhen (addr(4, 0) === UInt(28)) {
    nextPCOffset := UInt(4)
  } .otherwise {
    nextPCOffset := UInt(16)
  }

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

  io.fetchBlockPC := io.icacheRespAddr

  for (i <- 0 until 4) {
    io.insts(i) := io.icacheRespValues(i)
  }
}

class FetchTests(c: Fetch) extends Tester(c) { 
} 

class FetchGenerator extends TestGenerator {
  def genMod(): Module = Module(new Fetch())
  def genTest[T <: Module](c: T): Tester[T] = 
    (new FetchTests(c.asInstanceOf[Fetch])).asInstanceOf[Tester[T]]
}
