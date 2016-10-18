package riscy

import Chisel._

/**
 * A parametrized register file.
 *
 * The value of a register can be read in the same cycle by changing the value
 * of one of the read ports. The value of a register can be written on the clock
 * edge in one cycle.
 *
 * To Read: Set one of the rPorts to the register number you wish to read. Then,
 * the value will appear on the corresponding rValues wire.
 *
 * To Write: Set one of the wPorts to the register number you wish to write. Set
 * the corresponding valid bit and wValues.
 *
 * Read ports are cheap. Write ports are expensive.
 *
 * @param width the bit width of each register.
 * @param size the number of registers.
 * @param numRPorts the number of read ports.
 * @param numWPorts the number of write ports.
 */
class RegFile(width: Int, size: Int, numRPorts: Int, numWPorts: Int) extends Module {
  val io = new Bundle {
    val rPorts = Vec.fill(numRPorts) { UInt(INPUT, 5) }
    val rValues = Vec.fill(numRPorts) { UInt(OUTPUT, 32) }

    val wPorts = Vec.fill(numWPorts) { UInt(INPUT, 5) }
    val wVs = Vec.fill(numWPorts) { Bool(INPUT) }
    val wValues = Vec.fill(numWPorts) { UInt(INPUT, 32) }
  }

  // The actual registers
  val regs = Array.tabulate(size) { i => UInt(i) -> Reg(UInt(width)) }

  // Hook up read ports with muxes to regs
  for (i <- 0 until numRPorts) {
    io.rValues(i) := MuxLookup(io.rPorts(i), UInt(0), regs)
  }

  // Hook up write ports to regs
  for (p <- 0 until numWPorts) {
    for (r <- 0 until size) {
      when (io.wVs(p) && io.wPorts(p) === UInt(r)) {
        regs(r)._2 := io.wValues(p)
      }
    }
  }
}

class RegFileTests(c: RegFile) extends Tester(c) {
  for (i <- 0 until 1000) {
    val randVal = rnd.nextInt(1 << 16)
    val randRPort = rnd.nextInt(4)
    val randWPort = rnd.nextInt(4)
    val randReg = rnd.nextInt(16)

    // Write to the registers
    for (p <- 0 until 4) {
      if (p == randWPort) {
        poke(c.io.wPorts(randWPort), randReg)
        poke(c.io.wVs(randWPort), 1)
        poke(c.io.wValues(randWPort), randVal)
      } else {
        poke(c.io.wVs(p), 0)
      }
    }

    // Then, read back
    step(1)
    poke(c.io.rPorts(randRPort), randReg)

    step(0)
    expect(c.io.rValues(randRPort), randVal)
  }
}

class RegFileGenerator extends TestGenerator {
  def genMod(): Module = Module(new RegFile(16, 16, 4, 4))
  def genTest[T <: Module](c: T): Tester[T] =
    (new RegFileTests(c.asInstanceOf[RegFile])).asInstanceOf[Tester[T]]
}
