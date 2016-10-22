package riscy

import Chisel._

// Information from the Allocate/Rename stage to the Remap table.
class AllocRemap extends Bundle {
  val reg = UInt(OUTPUT, 5) // which reg to rename
  val idxROB = UInt(OUTPUT, 6) // ROB entry number to map to
}

// Information from the Allocate/Rename stage to the ROB and IQs.
class AllocROB extends ROBEntry {
  // Which ROB entry to latch
  val entry = UInt(OUTPUT, 6)

  // Is the operand coming from the ROB? T => Y, F => N
  val rs1Map = Bool(OUTPUT)
  val rs2Map = Bool(OUTPUT)
}

// The Control Magic to rename instructions and send signals to update the
// Remap table, the ROB, and the Issue Queues.
class RiscyAlloc extends Module {
  val io = new Bundle {
    // Input from the rotator and decode logic with 4 decoded instructions
    val inst = Vec.fill(4) { Valid(new DecodeIns()).flip }

    // Access the Remap table to find out what the current mappings are (so we
    // can rename)
    val remapPorts = Vec.fill(8) { UInt(OUTPUT, 5) }
    val remapMapping = Vec.fill(8) { Valid(UInt(INPUT, 6)) }

    // Arch register access is needed to get reg value for ROB entry
    val rfPorts = Vec.fill(8) { UInt(OUTPUT, 5) }
    val rfValues = Vec.fill(8) { UInt(INPUT, 32) }

    // ROB table access to populate next ROB entry
    val robPorts = Vec.fill(8) { UInt(OUTPUT, 6) }
    val robDest = Vec.fill(8) { Valid(UInt(INPUT, 32)) }
    val robSpec = Bool(INPUT) // Is the last inst speculative?
    val robFree = UInt(INPUT, 6) // How many free entries
    val robFirst = UInt(INPUT, 6) // Index of the first free entry

    // Outputs to the Remap table and the ROB with the correct values to update
    // for this cycle. 
    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()) }
    val allocROB = Vec.fill(4) { Valid(new AllocROB()) }
  }

  // TODO: will need some latches to break this into two stages

  // TODO: stall if ROB is full

  // For each instruction, determine what resources/registers it needs.
  val opDecodes = Array.tabulate(4) {
    i => {
      val od = Module(new RiscyOpDecode())
      od.io.op := io.inst(i).bits.op
      od
    }
  }

  // Do a simple addition to rename the instructions. Every instruction gets
  // an ROB entry, regardless of how many registers it reads or writes. The
  // valid bits of the instructions and the number of free ROB entries determines
  // whether we should stall and how many entries we should put into the ROB.
  val renamedDest = Vec.tabulate(4) { i => io.robFirst + UInt(i, 2) }

  // Hook up instructions to remap table
  for (i <- 0 until 4) {
    io.remapPorts(2*i) := io.inst(i).bits.rs1
    io.remapPorts(2*i+1) := io.inst(i).bits.rs2
  }

  // Rename all src operands
  val renamedRs1 = Vec.fill(4) { Valid(UInt(width = 6)) }
  val renamedRs2 = Vec.fill(4) { Valid(UInt(width = 6)) }
  for (i <- 0 until 4) {
    if (i == 0) {
      // from remap table if i is 0
      renamedRs1(i) := io.remapMapping(2*i)
      renamedRs2(i) := io.remapMapping(2*i+1)
    } else {
      // Either a previous instructions renames the register or the proper
      // renaming info can be found in the remap table.
      for (j <- 0 until i) {
        // rs1
        when (io.inst(j).bits.rs1 === io.inst(i).bits.rd) {
          // take i's mapping
          renamedRs1(j).valid := Bool(true)
          renamedRs1(j).bits := renamedDest(i)
        } .otherwise {
          // remap table
          renamedRs1(j).valid := io.remapMapping(2*j).valid
          renamedRs1(j).bits := io.remapMapping(2*j).bits
        }

        // rs2
        when (io.inst(j).bits.rs2 === io.inst(i).bits.rd) {
          // take i's mapping
          renamedRs2(j).valid := Bool(true)
          renamedRs2(j).bits := renamedDest(i)
        } .otherwise {
          // remap table
          renamedRs2(j).valid := io.remapMapping(2*j+1).valid
          renamedRs2(j).bits := io.remapMapping(2*j+1).bits
        }
      }
    }
  }

  // Now, hook up the ouputs
  for (i <- 0 until 4) {
    // Valid bits for ROB: each valid instruction results in a valid ROB entry
    io.allocROB(i).valid := io.inst(i).valid

    val robEntry = io.allocROB(i).bits // convenience

    // Which ROB entry
    robEntry.entry := renamedDest(i)

    // Bulk wire data from decoded instruction
    robEntry.op := io.inst(i).bits.op
    robEntry.funct3 := io.inst(i).bits.funct3
    robEntry.funct7 := io.inst(i).bits.funct7

    // First operand
    when (renamedRs1(i).valid) {
      // Getting from ROB
      robEntry.rs1Map := Bool(true)
      robEntry.rs1Rename := renamedRs1(i).bits
      robEntry.rs1Val.valid := io.robDest(renamedRs1(i).bits).valid
      robEntry.rs1Val.bits := io.robDest(renamedRs1(i).bits).bits
    } .otherwise {
      // Getting from Arch Reg File
      robEntry.rs1Map := Bool(false)
      robEntry.rs1Rename := io.inst(i).bits.rs1
      robEntry.rs1Val.valid := Bool(true)
      robEntry.rs1Val.bits := io.rfValues(io.inst(i).bits.rs1)
    }

    // Second operand
    when (opDecodes(i).io.opInfo.hasRs2) {
      // Second operand is a register
      when (renamedRs2(i).valid) {
        // Getting from ROB
        robEntry.rs2Map := Bool(true)
        robEntry.rs2Rename := renamedRs2(i).bits
        robEntry.rs2Val.valid := io.robDest(renamedRs2(i).bits).valid
        robEntry.rs2Val.bits := io.robDest(renamedRs2(i).bits).bits
      } .otherwise {
        // Getting from Arch Reg File
        robEntry.rs2Map := Bool(false)
        robEntry.rs2Rename := io.inst(i).bits.rs2
        robEntry.rs2Val.valid := Bool(true)
        robEntry.rs2Val.bits := io.rfValues(io.inst(i).bits.rs2)
      }
    } .otherwise {
      // Second operand is an immediate
      robEntry.rs2Map := Bool(false)
      robEntry.rs2Rename := UInt(0)
      robEntry.rs2Val.valid := Bool(true)
      robEntry.rs2Val.bits := UInt(0) // chisel requires this for some reason :(

      // Choose which immediate
      when (opDecodes(i).io.opInfo.hasImmI) {
        robEntry.rs2Val.bits := io.inst(i).bits.immI
      } .elsewhen (opDecodes(i).io.opInfo.hasImmS) {
        robEntry.rs2Val.bits := io.inst(i).bits.immS
      } .elsewhen (opDecodes(i).io.opInfo.hasImmB) {
        robEntry.rs2Val.bits := io.inst(i).bits.immB
      } .elsewhen (opDecodes(i).io.opInfo.hasImmU) {
        robEntry.rs2Val.bits := io.inst(i).bits.immU
      } .otherwise { // J Immediate
        robEntry.rs2Val.bits := io.inst(i).bits.immJ
      }
    }

    // This instruction is speculative if either operand is speculative...
    // TODO: If we do this, then we need to account for speculative instructions
    // among the four we are decoding...
    // NOTE: For now, speculative if last instruction was speculative
    robEntry.spec := io.robSpec

    // Remap entries:
    // - a remap entry should be written if the instruction has a destination
    // register and no later instruction is renaming the same arch reg
    //
    // - the value of the remap entry is the ROB entry number
    io.allocRemap(i).valid := opDecodes(i).io.opInfo.hasRd &&
      io.inst(i).valid &&
      io.allocRemap
        .slice(i+1, io.allocRemap.size)
        .foldLeft(Bool(true)) {
          // j is the next allocRemap entry being considered
          // i is valid if j is not or if j maps a different register
          (total, j) => total && (!j.valid || j.bits.reg != io.allocRemap(i).bits.reg)
        }
    io.allocRemap(i).bits.reg := io.inst(i).bits.rd
    io.allocRemap(i).bits.idxROB := renamedDest(i)
  }
}

class RiscyAllocTests(c: RiscyAlloc) extends Tester(c) {
  // TEST 1
  // add r1 <- r1 + 0xFFF
  // add r2 <- r1 + 0xFFF

  poke(c.io.inst(0).valid, 1)
  poke(c.io.inst(0).bits.op, 0x33)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x1)
  poke(c.io.inst(0).bits.rd, 0x1)
  poke(c.io.inst(0).bits.immI, 0xFFF)

  poke(c.io.inst(1).valid, 1)
  poke(c.io.inst(1).bits.op, 0x33)
  poke(c.io.inst(1).bits.funct3, 0x0)
  poke(c.io.inst(1).bits.rs1, 0x1)
  poke(c.io.inst(1).bits.rd, 0x2)
  poke(c.io.inst(1).bits.immI, 0xFFF)

  poke(c.io.inst(2).valid, 0)
  poke(c.io.inst(3).valid, 0)

  poke(c.io.robFree, 64)
  poke(c.io.robFirst, 0)

  step(1)

  // TODO: expect output to ROB

  // Should map r1 to ROB0
  expect(c.io.allocRemap(0).valid, 1)
  expect(c.io.allocRemap(0).bits.reg, 1)
  expect(c.io.allocRemap(0).bits.idxROB, 0)

  // Should map r2 to ROB1
  expect(c.io.allocRemap(1).valid, 1)
  expect(c.io.allocRemap(1).bits.reg, 2)
  expect(c.io.allocRemap(1).bits.idxROB, 1)

  expect(c.io.allocRemap(2).valid, 0)
  expect(c.io.allocRemap(3).valid, 0)


  // TEST2
  // add r1 <- r1 + 0xFFF
  // add r1 <- r1 + 0xFFF
  // add r1 <- r1 + 0xFFF
  // add r1 <- r1 + 0xFFF

  poke(c.io.inst(0).valid, 1)
  poke(c.io.inst(0).bits.op, 0x33)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x1)
  poke(c.io.inst(0).bits.rd, 0x1)
  poke(c.io.inst(0).bits.immI, 0xFFF)

  poke(c.io.inst(1).valid, 1)
  poke(c.io.inst(1).bits.op, 0x33)
  poke(c.io.inst(1).bits.funct3, 0x0)
  poke(c.io.inst(1).bits.rs1, 0x1)
  poke(c.io.inst(1).bits.rd, 0x1)
  poke(c.io.inst(1).bits.immI, 0xFFF)

  poke(c.io.inst(2).valid, 1)
  poke(c.io.inst(2).bits.op, 0x33)
  poke(c.io.inst(2).bits.funct3, 0x0)
  poke(c.io.inst(2).bits.rs1, 0x1)
  poke(c.io.inst(2).bits.rd, 0x1)
  poke(c.io.inst(2).bits.immI, 0xFFF)

  poke(c.io.inst(3).valid, 1)
  poke(c.io.inst(3).bits.op, 0x33)
  poke(c.io.inst(3).bits.funct3, 0x0)
  poke(c.io.inst(3).bits.rs1, 0x1)
  poke(c.io.inst(3).bits.rd, 0x1)
  poke(c.io.inst(3).bits.immI, 0xFFF)

  poke(c.io.robFree, 62)
  poke(c.io.robFirst, 2)

  step(1)

  // TODO: expect output to ROB

  // Should map r1 to ROB5
  expect(c.io.allocRemap(3).valid, 1)
  expect(c.io.allocRemap(3).bits.reg, 1)
  expect(c.io.allocRemap(3).bits.idxROB, 5)

  expect(c.io.allocRemap(0).valid, 0)
  expect(c.io.allocRemap(1).valid, 0)
  expect(c.io.allocRemap(2).valid, 0)

  // TEST 3: no valid instructions
  poke(c.io.inst(0).valid, 0)
  poke(c.io.inst(1).valid, 0)
  poke(c.io.inst(2).valid, 0)
  poke(c.io.inst(3).valid, 0)

  step(1)

  expect(c.io.allocRemap(0).valid, 0)
  expect(c.io.allocRemap(1).valid, 0)
  expect(c.io.allocRemap(2).valid, 0)
  expect(c.io.allocRemap(3).valid, 0)

  // TEST 4
  // add r1 <- r1 + 0xFFF
  // add r2 <- r1 + 0xFFF
  // add r2 <- r1 + 0xFFF
  poke(c.io.inst(0).valid, 1)
  poke(c.io.inst(0).bits.op, 0x33)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x1)
  poke(c.io.inst(0).bits.rd, 0x1)
  poke(c.io.inst(0).bits.immI, 0xFFF)

  poke(c.io.inst(1).valid, 1)
  poke(c.io.inst(1).bits.op, 0x33)
  poke(c.io.inst(1).bits.funct3, 0x0)
  poke(c.io.inst(1).bits.rs1, 0x1)
  poke(c.io.inst(1).bits.rd, 0x2)
  poke(c.io.inst(1).bits.immI, 0xFFF)

  poke(c.io.inst(2).valid, 1)
  poke(c.io.inst(2).bits.op, 0x33)
  poke(c.io.inst(2).bits.funct3, 0x0)
  poke(c.io.inst(2).bits.rs1, 0x1)
  poke(c.io.inst(2).bits.rd, 0x2)
  poke(c.io.inst(2).bits.immI, 0xFFF)

  poke(c.io.inst(3).valid, 0)

  poke(c.io.robFree, 58)
  poke(c.io.robFirst, 6)

  step(1)

  // TODO: expect output to ROB

  // Should map r1 to ROB6
  expect(c.io.allocRemap(0).valid, 1)
  expect(c.io.allocRemap(0).bits.reg, 1)
  expect(c.io.allocRemap(0).bits.idxROB, 6)

  // Should map r2 to ROB8
  expect(c.io.allocRemap(2).valid, 1)
  expect(c.io.allocRemap(2).bits.reg, 2)
  expect(c.io.allocRemap(2).bits.idxROB, 8)

  expect(c.io.allocRemap(1).valid, 0)
  expect(c.io.allocRemap(3).valid, 0)

  // TODO: add more tests
}

class AllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyAllocTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
}
