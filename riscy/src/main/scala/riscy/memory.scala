package riscy

import Chisel._

class BigMemory(lineBytes: Int, numLines: Int) extends Module {
  val io = new Bundle {
    // TODO: interface with cache
    val readAddr = Valid(UInt(INPUT, 64))
    val readData = Valid(UInt(OUTPUT, lineBytes*8))
  }

  // TODO: make it have 100 cyc delay

  println("Creating BigMemory of size" + ((lineBytes * numLines) >> 10) + " kB")

  val memBank = Mem(Bits(width = 8 * lineBytes), numLines)


  // TODO: actually create a read interface...
  io.readAddr.valid := io.readData.valid
  io.readData.bits := memBank(io.readAddr.bits)
}
