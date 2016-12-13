package riscy

import Chisel._

class BigMemory(lineBytes: Int, numLines: Int, numRPorts: Int, numWPorts: Int, latency: Int) extends Module {
  val io = new Bundle {
    val readPorts = Vec.fill(numRPorts) { Valid(UInt(INPUT, 64)).asInput }
    val readData = Vec.fill(numRPorts) { Valid(UInt(OUTPUT, lineBytes*8)).asOutput }
    val readCancel = Vec.fill(numRPorts) { Valid(UInt(INPUT, 64)).asInput }

    val writePorts = Vec.fill(numWPorts) { Valid(UInt(INPUT, 64)).asInput }
    val writeData = Vec.fill(numWPorts) { UInt(INPUT, 64).asInput }
    val writeSize = Vec.fill(numWPorts) { UInt(INPUT, 3).asInput }
  }

  println("Creating BigMemory of size " + ((lineBytes * numLines) >> 10) + " kB")

  val memBank = Mem(UInt(width = 8), numLines * lineBytes)

  // Need the ability to cancel reads
  val cancelLatch = Vec.fill(numRPorts) { Reg(Valid(UInt(width = 64))) }

  // Read ports
  for(i <- 0 until numRPorts) {
    val reqDelayed = if(latency > 0) {
      Pipe(io.readPorts(i), latency)
    } else {
      io.readPorts(i)
    }

    val data = UInt(width = lineBytes * 8)
    data := UInt(0)

    when(reqDelayed.valid) {
      val dataBytes = Array.tabulate(lineBytes) {
        j => memBank(reqDelayed.bits + UInt(j))
      }

      for(j <- 0 until lineBytes) {
        data((j+1)*8-1,j*8) := dataBytes(j)
      }
    } .otherwise {
      data := UInt(0) // Please chisel with a default value
    }

    val shouldCancel = reqDelayed.bits === cancelLatch(i).bits && cancelLatch(i).valid

    // Assumes that we will never cancel two different reads at the same time
    // from the same port
    when(io.readCancel(i).valid) {
      cancelLatch(i).valid := io.readCancel(i).valid
      cancelLatch(i).bits := io.readCancel(i).bits
    } .elsewhen (shouldCancel) {
      cancelLatch(i).valid := Bool(false)
      cancelLatch(i).bits := UInt(0)
    }

    io.readData(i).valid := reqDelayed.valid && !shouldCancel
    io.readData(i).bits := data
  }

  // Hook up write ports
  for(i <- 0 until numWPorts) {
    val reqDelayed = {
      val wdata = Valid(new WriteData)
      wdata.valid := io.writePorts(i).valid
      wdata.bits.addr := io.writePorts(i).bits
      wdata.bits.data := io.writeData(i)
      wdata.bits.size := io.writeSize(i)
      Pipe(wdata, latency-1)
    }

    when(reqDelayed.valid) {
      when(reqDelayed.bits.size === UInt(0)) { // lb -- 1B
        for(j <- 0 until 1) {
          memBank(reqDelayed.bits.addr + UInt(j)) := reqDelayed.bits.data((j+1)*8-1,j*8)
        }
      } .elsewhen(reqDelayed.bits.size === UInt(1)) { // lh -- 2B
        for(j <- 0 until 2) {
          memBank(reqDelayed.bits.addr + UInt(j)) := reqDelayed.bits.data((j+1)*8-1,j*8)
        }
      } .elsewhen(reqDelayed.bits.size === UInt(2)) { // lw -- 4B
        for(j <- 0 until 4) {
          memBank(reqDelayed.bits.addr + UInt(j)) := reqDelayed.bits.data((j+1)*8-1,j*8)
        }
      } .elsewhen(reqDelayed.bits.size === UInt(3)) { // ld -- 8B
        for(j <- 0 until 8) {
          memBank(reqDelayed.bits.addr + UInt(j)) := reqDelayed.bits.data((j+1)*8-1,j*8)
        }
      }

      printf("MEM[%x] = %x\n", reqDelayed.bits.addr, reqDelayed.bits.data)
    }
  }
}

class WriteData extends Bundle {
  val addr = UInt(INPUT, 64)
  val data = UInt(INPUT, 64)
  val size = UInt(INPUT,  3)
}

class MemoryTests(c: BigMemory) extends Tester(c) {
  def wait(n: Int) {
    for(i <- 0 until n) {
      expect(c.io.readData(0).valid, false)
      step(1)
    }
  }

  // Read address 0x0
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0)

  // Read address 0x10
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0)

  // Write address 0x0
  poke(c.io.writePorts(0).valid, true)
  poke(c.io.writePorts(0).bits, 0x0)
  poke(c.io.writeSize(0), 0x0) // sb
  poke(c.io.writeData(0), 0xDEADBEEF)

  step(1)

  poke(c.io.writePorts(0).valid, false)

  // Read address 0x0
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xEF)

  // Read again
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xEF)

  // Write address 0x0
  poke(c.io.writePorts(0).valid, true)
  poke(c.io.writePorts(0).bits, 0x0)
  poke(c.io.writeSize(0), 0x1) // sh
  poke(c.io.writeData(0), 0xDEADBEEF)

  step(1)

  poke(c.io.writePorts(0).valid, false)


  // Read address 0x0
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xBEEF)

  // Write address 0x0
  poke(c.io.writePorts(0).valid, true)
  poke(c.io.writePorts(0).bits, 0x0)
  poke(c.io.writeSize(0), 0x2) // sw
  poke(c.io.writeData(0), 0xDEADBEEF)

  step(1)

  poke(c.io.writePorts(0).valid, false)

  // Read address 0x0
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xDEADBEEF)

  // Write address 0x0
  poke(c.io.writePorts(0).valid, true)
  poke(c.io.writePorts(0).bits, 0x0)
  poke(c.io.writeSize(0), 0x3) // sd
  poke(c.io.writeData(0), 0xCAFEBABEDEADBEEFl)

  step(1)

  poke(c.io.writePorts(0).valid, false)

  // Read address 0x0
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  // Read address 0x4
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 4)

  step(1)

  poke(c.io.readPorts(0).valid, false)

  wait(2)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xDEADBEEF)

  step(1)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xCAFEBABE)

  // Read address 0x0 and 0x1 but cancel the read for 0x0
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 4)

  poke(c.io.readCancel(0).valid, true)
  poke(c.io.readCancel(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)
  poke(c.io.readCancel(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xCAFEBABE)

  // Cancel an addr but then request it again
  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, true)
  poke(c.io.readPorts(0).bits, 0)

  poke(c.io.readCancel(0).valid, true)
  poke(c.io.readCancel(0).bits, 0)

  step(1)

  poke(c.io.readPorts(0).valid, false)
  poke(c.io.readCancel(0).valid, false)

  wait(3)

  expect(c.io.readData(0).valid, true)
  expect(c.io.readData(0).bits, 0xDEADBEEF)

}

class MemoryGenerator extends TestGenerator {
  def genMod(): Module = Module(new BigMemory(4, 16, 1, 1, 4))
  def genTest[T <: Module](c: T): Tester[T] =
    (new MemoryTests(c.asInstanceOf[BigMemory])).asInstanceOf[Tester[T]]
}
