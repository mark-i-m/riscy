package riscy

import Chisel._

// The whole processor!
class Riscy extends Module {
  val io = new Bundle { /* No system, just a processor! */ }

  var memory = Module(new BigMemory(64, 1 << 24)) // 1 gB memory

  // TODO: wire things up
  //val bp = Module(new BP)
  var fetch = Module(new Fetch)
  var icache = Module(new ICache)
  val alloc = Module(new RiscyAlloc)
  val rob = Module(new ROB)
  //val issue = Module(new IssueStage)
  //val lsq = Module(new LSQ)
  //val fus... execution?
}
