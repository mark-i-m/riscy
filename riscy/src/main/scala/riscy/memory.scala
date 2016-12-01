package riscy

import Chisel._

class BigMemory(lineBytes: Int, numLines: Int, numRPorts: Int, numWPorts: Int, latency: Int) extends Module {
  val io = new Bundle {
    val readPorts = Vec.fill(numRPorts) { Valid(UInt(INPUT, 64)).asInput }
    val readData = Vec.fill(numRPorts) { Valid(UInt(OUTPUT, lineBytes*8)).asOutput }

    val writePorts = Vec.fill(numWPorts) { Valid(UInt(INPUT, 64)) }
    val writeData = Vec.fill(numWPorts) { UInt(INPUT, 64) }
  }

  println("Creating BigMemory of size " + ((lineBytes * numLines) >> 10) + " kB")

  val memBank = Mem(Bits(width = 8 * lineBytes), numLines)

  // Hook up read ports
  val readValue = Vec.fill(numRPorts) { Valid(UInt(width = 64)) }

  for(i <- 0 until numRPorts) {
    readValue(i).valid := io.readPorts(i).valid
    readValue(i).bits := memBank(io.readPorts(i).bits)

    if(latency > 0) {
      io.readData(i) := Pipe(readValue(i), latency)
    } else {
      io.readData(i) := readValue(i)
    }
  }

  // Hook up write ports
  val wPipes = Array.tabulate(numWPorts) { i => {
    val wdata = Valid(new WriteData)
    wdata.valid := io.writePorts(i).valid
    wdata.bits.addr := io.writePorts(i).bits
    wdata.bits.data := io.writeData(i)
    Pipe(wdata)
  }}

  for(i <- 0 until numWPorts) {
    when(wPipes(i).valid) {
      memBank(wPipes(i).bits.addr) := wPipes(i).bits.data
    }
  }
}

class WriteData extends Bundle {
  val addr = UInt(INPUT, 64)
  val data = UInt(INPUT, 64)
}
