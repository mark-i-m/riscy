package riscy

import Chisel._

class BigMemory(lineBytes: Int, numLines: Int) extends Module {
  val io = new Bundle {
    // TODO: interface with cache
  }

  // TODO: make it have 100 cyc delay

  println("Creating BigMemory of size" + ((lineBytes * numLines) >> 10) + " kB")

  val memBank = Mem(Bits(width = 8 * lineBytes), numLines)
}
