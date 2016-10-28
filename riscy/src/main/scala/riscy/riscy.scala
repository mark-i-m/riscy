package riscy

import Chisel._

// The whole processor!
class Riscy extends Module {
  val io = new Bundle {
    // TODO
  }

  // TODO: wire things up
  var fetch = new Fetch();
  var icache = new ICache();
  val alloc = new Alloc();
  val rob = new ROB();
}
