package riscy

import Chisel._

// The whole processor!
class Riscy extends Module {
  val io = new Bundle { /* No system, just a processor! */ }

  var memory = Module(new BigMemory(64, 1 << 24, 2, 2, 100)) // 1 gB memory

  val bp = Module(new BP)
  var fetch = Module(new Fetch)
  val decode = Array.fill(4)(Module(new RiscyDecodeSingle))
  val alloc = Module(new RiscyAlloc)
  val rob = Module(new ROB)
  val issue = Module(new IssueStage)
  val lsq = Module(new LSQ)
  val exec = Array.fill(4)(Module(new Execute))
  val stall = Module(new Stall)

  // TODO: hook up ICache and Memory
  fetch <> memory

  // TODO: hook up BP and Fetch
  fetch <> bp

  // branch misprediction signals from ROB to Fetch
  fetch.io.isBranchMispred := rob.io.mispredPC.valid
  fetch.io.branchMispredTarget := rob.io.mispredTarget

  for(i <- 0 until 4) {
    // instructions from Fetch to Decode
    decode(i).io.ins := fetch.io.insts(i)

    // decoded instructions to Allocation
    alloc.io.inst(i) := decode(i).io.decoded
  }

  // Hook up all signals between Allocation and ROB
  alloc <> rob

  // TODO: Hook up Allocation and IssueStage
  alloc <> issue

  // TODO: Hook up IssueStage and LSQ (Addr Q)
  issue <> lsq

  // TODO: Hook up ROB WB and ROB
  exec <> rob

  // Hook up LSQ to ROB, so we can commit stores
  lsq.io.stCommit := rob.io.stCommit

  // TODO: hook up DCache and Memory
  lsq <> memory
}
