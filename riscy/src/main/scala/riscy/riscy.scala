package riscy

import Chisel._

// The whole processor!
class Riscy extends Module {
  val io = new Bundle { /* No system, just a processor! */ }

  //var memory = Module(new BigMemory(64, 1 << 24, 2, 2, 100)) // 1 gB memory
  var memory = Module(new BigMemory(64, 1 << 12, 2, 2, 100)) // 256 kB memory

  // TODO add BP
  //val bp = Module(new BP)
  var fetch = Module(new Fetch)
  val decode = Array.fill(4)(Module(new DecodeSingle))
  val alloc = Module(new RiscyAlloc)
  val rob = Module(new ROB)
  val issue = Module(new Issue)
  val lsq = Module(new LSQ)
  val exec = Module(new Execute)
  val stall = Module(new Stall)

  // TODO: hook up ICache and Memory
  // fetch <> memory

  // TODO: hook up BP and Fetch
  //fetch <> bp

  // branch misprediction signals from ROB to Fetch
  fetch.io.isBranchMispred := rob.io.mispredPC.valid
  fetch.io.branchMispredTarget := rob.io.mispredTarget

  for(i <- 0 until 4) {
    // instructions from Fetch to Decode
    decode(i).io.ins := fetch.io.output.insts(i)
    decode(i).io.pc  := fetch.io.output.pc(i)

    // decoded instructions to Allocation
    // NOTE: decode and alloc are part of a single pipeline stage, so passing
    // PC from fetch directly to alloc is ok.
    alloc.io.inst(i) := decode(i).io.decoded
    alloc.io.pc(i)   := fetch.io.output.pc(i)
  }

  // Hook up all signals between Allocation and ROB
  alloc <> rob

  // Hook up Allocation and IssueStage
  // ROB entries come directly from Alloc
  issue.io.inst := alloc.io.allocROB

  // Hook up IssueStage and LSQ (Addr Q)
  lsq.io.resEntry     := issue.io.addBuf
  issue.io.addBufLen  := lsq.io.currentLen

  // TODO: Hook up Exec and ROB

  // TODO: LSQ and Exec

  // TODO: Issue and Exec
  exec.io.issued_inst := issue.io.issuedEntry

  // Hook up LSQ to ROB, so we can commit stores
  lsq.io.stCommit := rob.io.stCommit

  // TODO: hook up DCache and Memory
  //lsq <> memory

  // Hook up stalling logic
  fetch.io.stall := stall.io.fetchStall
  alloc <> stall
  rob <> stall

  stall.io.arbiterStallReq := issue.io.stall
}

class TopLevelTests(c: Riscy) extends Tester(c) {
}

class TopLevelGenerator extends TestGenerator {
  def genMod(): Module = Module(new Riscy)
  def genTest[T <: Module](c: T): Tester[T] =
    (new TopLevelTests(c.asInstanceOf[Riscy])).asInstanceOf[Tester[T]]
}
