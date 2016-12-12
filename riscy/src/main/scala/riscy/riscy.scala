package riscy

import Chisel._

// The whole processor!
class Riscy(blackbox: Boolean = false) extends Module {
  val io = new Bundle {
    /* No system, just a processor! */
    val cycles = UInt(OUTPUT, width = 128)

    // For testing and debugging
    val ins = Vec(4, Valid(UInt(INPUT, 32))).asInput
  }

  // Cycle counter -- purely for debugging
  val cycles = Reg(init = UInt(0, width = 32))
  cycles := cycles + UInt(1)
  io.cycles := cycles

  printf("\n---------------------- CYCLE %d ----------------------\n", cycles)

  // Memory for both data and instructions
  // - Port 0 => Instruction/Fetch
  // - Port 1 => Data/LSQ
  var memory = Module(new BigMemory(64, 1 << 10, 2, 2, 5)) // 64kB memory, 8 word cache lines, 5 cycle latency

  val bp = Module(new BP)
  var fetch = Module(new Fetch)
  val decode = Array.fill(4)(Module(new DecodeSingle))
  val alloc = Module(new RiscyAlloc)
  val rob = Module(new ROB)
  val issue = Module(new Issue)
  val lsq = Module(new LSQ)
  val exec = Module(new Execute)
  val stall = Module(new Stall)

  // Hook up ICache and Memory
  memory.io.readPorts(0) := fetch.io.memReadPort
  memory.io.readCancel(0) := fetch.io.memCancelPort
  fetch.io.memReadData := memory.io.readData(0)

  // Hook up BP and Fetch
  fetch <> bp

  // branch misprediction signals from ROB to Fetch
  fetch.io.isBranchMispred := rob.io.mispredPC.valid
  fetch.io.branchMispredTarget := rob.io.mispredTarget

  for(i <- 0 until 4) {
    // If testing as a blackbox, use our fetch unit and memory;
    // otherwise, poke instructions into decode directly.
    if(blackbox) {
      // instructions from Fetch to Decode
      decode(i).io.ins := fetch.io.output.insts(i)
      decode(i).io.pc  := fetch.io.output.pc(i)
    } else {
      decode(i).io.ins := io.ins(i)
    }

    // decoded instructions to Allocation
    // NOTE: decode and alloc are part of a single pipeline stage, so passing
    // PC from fetch directly to alloc is ok.
    alloc.io.inst(i) := decode(i).io.decoded

    if(blackbox) {
      alloc.io.pc(i) := fetch.io.output.pc(i)
      alloc.io.bchPredTaken(i) := fetch.io.output.predTaken(i)
    }
  }

  // Hook up all signals between Allocation and ROB
  alloc <> rob

  // Hook up Allocation and IssueStage
  // ROB entries come directly from Alloc
  issue.io.inst := alloc.io.allocROB

  //ROB to issue stage era
  issue.io.era := rob.io.robEra
	
  // Hook up IssueStage and LSQ (Addr Q)
  lsq.io.resEntry     := issue.io.addrBuf
  issue.io.addrBufLen  := lsq.io.currentLen

  // Hook up Exec and ROB
  exec.io.rob_era := rob.io.robEra
  for(i <- 0 until 6) {
    rob.io.wbValues(i) := exec.io.rob_wb_output.entry(i)
  }

  // LSQ and Exec
  lsq.io.robWbin := exec.io.rob_wb_store
  exec.io.lsq_input <> lsq.io.robWbOut

  // Issue and Exec
  exec.io.issued_inst := issue.io.issuedEntry
  exec.io.specIssue   := issue.io.specIssue
  issue.io.robWb <> exec.io.rob_wb_store

  // Hook up LSQ to ROB, so we can commit stores
  // Can only store up to two values per cycle
  lsq.io.stCommit(0) := rob.io.stCommit(0)
  lsq.io.stCommit(1) := rob.io.stCommit(1)

  // Hook up DCache and Memory
  for(i <- 0 until 2) { // Write ports
    memory.io.writePorts(i) := lsq.io.memStAddrPort(i)
    memory.io.writeData(i) := lsq.io.memStData(i)
    memory.io.writeSize(i) := lsq.io.memStSize(i)
  }

  memory.io.readPorts(1) := lsq.io.memLdAddrPort
  lsq.io.memLdData := memory.io.readData(1)

  // Hook up stalling logic
  fetch.io.stall      := stall.io.fetchStall
  alloc.io.allocStall := stall.io.allocStall

  stall.io.robStallReq := rob.io.robStallReq
  stall.io.arbiterStallReq := issue.io.stall
}

class TopLevelTests(c: Riscy) extends Tester(c) {
  def genAddRI(rs: Int, rd: Int, imm: Int): Int =
    ((imm) << 20) | ((rs) << 15) | ((0) << 12) | ((rd) << 7) | 0x13

  poke(c.io.ins(0).bits, genAddRI(1,1,1))
  poke(c.io.ins(0).valid, true)

  poke(c.io.ins(1).bits, genAddRI(2,2,2))
  poke(c.io.ins(1).valid, true)

  poke(c.io.ins(2).bits, genAddRI(3,3,3))
  poke(c.io.ins(2).valid, true)

  poke(c.io.ins(3).bits, genAddRI(4,4,4))
  poke(c.io.ins(3).valid, true)

  step(1)

  poke(c.io.ins(0).bits, 0xFFFFFFFF)
  poke(c.io.ins(0).valid, true)

  poke(c.io.ins(1).bits, 0xFFFFFFFF)
  poke(c.io.ins(1).valid, true)

  poke(c.io.ins(2).valid, false)
  poke(c.io.ins(3).valid, false)

  step(1)

  poke(c.io.ins(0).valid, false)
  poke(c.io.ins(1).valid, false)

  step(30)

}

class TopLevelGenerator extends TestGenerator {
  def genMod(): Module = Module(new Riscy)
  def genTest[T <: Module](c: T): Tester[T] =
    (new TopLevelTests(c.asInstanceOf[Riscy])).asInstanceOf[Tester[T]]
}
