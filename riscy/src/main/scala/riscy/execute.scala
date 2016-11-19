package riscy

import Chisel._

class Execute extends Module {
  val io = new Bundle {
    // Instructions to execute
    val issuedEntry = Vec.fill(4) {Valid(new ROBEntry).asInput}

    // Written back values
    // TODO: ROB WB store
    // TODO: ROB WB output
    // TODO: Taken bit
    // TODO: ROB entry tag
    // TODO: Correct target addr
    // TODO: PC which caused misprediction
  }
}
