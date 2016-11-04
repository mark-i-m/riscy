package riscy

import Chisel._

// The whole processor!
class Riscy extends Module {
  val io = new Bundle { }

  var memory = Module(new BigMemory(64, 1 << 24)); // 1 gB memory

  // TODO: wire things up
  var fetch = Module(new Fetch());
  var icache = Module(new ICache());
  val alloc = Module(new RiscyAlloc());
  val rob = Module(new ROB());
}
