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
 * @param size the number of registers.
 * @param numRPorts the number of read ports.
 * @param numWPorts the number of write ports.
 * @param gen a function that takes an integer i and returns an instance of the
 * @param name the name for debug printing
 * @param which Chisel can't print aggregate types, so choose a subpart of each
 * reg to print. This should be the return value of `which`.
 * register type. i is the register number.
 */
class RegFile[T <: Data](size: Int, numRPorts: Int, numWPorts: Int, gen: Int => T, name: String = null, which: T => Data = identity[T] _) extends Module {
  val io = new Bundle {
    val rPorts = Vec.fill(numRPorts) { UInt(INPUT, log2Up(size)) }
    val rValues = Vec.tabulate(numRPorts) { i => gen(i).asOutput }

    val wPorts = Vec.fill(numWPorts) { Valid(UInt(width = log2Up(size))).asInput }
    val wValues = Vec.tabulate(numWPorts) { i => gen(i).asInput }

    val reset = Valid(gen(0)).asInput
  }

  // The actual registers
  val regs = Array.tabulate(size) { i => UInt(i) -> Reg(gen(i)) }

  // Hook up read ports with muxes to regs
  for (i <- 0 until numRPorts) {
    io.rValues(i) := MuxLookup(io.rPorts(i), gen(0), regs)
  }

  // Hook up write ports to regs
  for (p <- 0 until numWPorts) {
    for (r <- 0 until size) {
      when (io.wPorts(p).valid && io.wPorts(p).bits === UInt(r)) {
        regs(r)._2 := io.wValues(p)

        if(name != null) {
          printf(name + "[%d] = %x from port %d\n", UInt(r), which(io.wValues(p)), UInt(p))
        }
      }
    }
  }

  when(io.reset.valid) {
    for(r <- 0 until size) {
      regs(r)._2 := io.reset.bits
    }

    if(name != null) {
      printf(name + " reset to %x\n", which(io.reset.bits))
    }
  }
}

class RegFileTests(c: RegFile[ValidIO[UInt]]) extends Tester(c) {
  for (i <- 0 until 100) {
    val randVal = rnd.nextInt(1 << 16)
    val randValid = rnd.nextInt(2)
    val randRPort = rnd.nextInt(4)
    val randWPort = rnd.nextInt(4)
    val randReg = rnd.nextInt(16)

    // Write to the registers
    for (p <- 0 until 4) {
      if (p == randWPort) {
        poke(c.io.wPorts(randWPort).valid, true)
        poke(c.io.wPorts(randWPort).bits, randReg)
        poke(c.io.wValues(randWPort).valid, randValid)
        poke(c.io.wValues(randWPort).bits, randVal)
      } else {
        poke(c.io.wPorts(p).valid, false)
      }
    }

    // Then, read back
    step(1)
    poke(c.io.rPorts(randRPort), randReg)

    step(0)
    expect(c.io.rValues(randRPort).valid, randValid)
    expect(c.io.rValues(randRPort).bits, randVal)
  }

  // Test reset
  poke(c.io.reset.valid, true)
  poke(c.io.reset.bits.valid, false)
  poke(c.io.reset.bits.bits, 0x1234)
  for(p <- 0 until 4) {
    poke(c.io.wPorts(p).valid, false)
  }

  step(1)

  for(i <- 0 until 16) {
    poke(c.io.rPorts(0), i)

    step(0)
    expect(c.io.rValues(0).valid, false)
    expect(c.io.rValues(0).bits, 0x1234)
  }
}

class RegFileGenerator extends TestGenerator {
  def genMod(): Module = Module(new RegFile(16, 4, 4, i => Valid(UInt(OUTPUT, 16))))
  def genTest[T <: Module](c: T): Tester[T] =
    (new RegFileTests(c.asInstanceOf[RegFile[ValidIO[UInt]]])).asInstanceOf[Tester[T]]
}
