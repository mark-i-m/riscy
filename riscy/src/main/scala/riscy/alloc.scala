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
    val remapMapping = Vec.fill(8) { Valid(UInt(INPUT, 6)).asInput }

    // Arch register access is needed to get reg value for ROB entry
    val rfPorts = Vec.fill(8) { UInt(OUTPUT, 5) }
    val rfValues = Vec.fill(8) { UInt(INPUT, 32) }

    // ROB table access to populate next ROB entry
    val robPorts = Vec.fill(8) { UInt(OUTPUT, 6) }
    val robDest = Vec.fill(8) { Valid(UInt(INPUT, 32)).asInput }
    val robSpec = Bool(INPUT) // Is the last inst speculative?
    val robFree = UInt(INPUT, 6) // How many free entries
    val robFirst = UInt(INPUT, 6) // Index of the first free entry

    // Outputs to the Remap table and the ROB with the correct values to update
    // for this cycle. 
    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()) }
    val allocROB = Vec.fill(4) { Valid(new AllocROB()) }
  }

  // TODO: stall if ROB is full

  // For each instruction, determine what resources/registers it needs.
  val opDecodes = Array.tabulate(4) {
    i => {
      val od = Module(new RiscyOpDecode())
      od.io.op := io.inst(i).bits.op
      od
    }
  }
  
  // Implementing pipeline for Opdecode and inst as per pipeline stage definition
  val pipelinedOpDecode = Vec.tabulate(4) {
    i => Reg(next = opDecodes(i).io.opInfo)
  }

  val pipelinedInst = Vec.tabulate(4) {
    i => Reg(next = io.inst(i))
  }

  // Do a simple addition to rename the instructions. Every instruction gets
  // an ROB entry, regardless of how many registers it reads or writes. The
  // valid bits of the instructions and the number of free ROB entries determines
  // whether we should stall and how many entries we should put into the ROB.
  val renamedDest = Vec.tabulate(4) { i => io.robFirst + UInt(i, 2) }

  // Hook up instructions to remap table, register file
  for (i <- 0 until 4) {
    io.remapPorts(2*i) := pipelinedInst(i).bits.rs1
    io.remapPorts(2*i+1) := pipelinedInst(i).bits.rs2

    io.rfPorts(2*i) := pipelinedInst(i).bits.rs1
    io.rfPorts(2*i+1) := pipelinedInst(i).bits.rs2
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
        when (io.inst(i).bits.rs1 === pipelinedInst(j).bits.rd) {
          // take j's mapping
          renamedRs1(i).valid := Bool(true)
          renamedRs1(i).bits := renamedDest(j)
        } .otherwise {
          // remap table
          renamedRs1(i).valid := io.remapMapping(2*i).valid
          renamedRs1(i).bits := io.remapMapping(2*i).bits
        }

        // rs2
        when (io.inst(i).bits.rs2 === pipelinedInst(j).bits.rd) {
          // take j's mapping
          renamedRs2(i).valid := Bool(true)
          renamedRs2(i).bits := renamedDest(j)
        } .otherwise {
          // remap table
          renamedRs2(i).valid := io.remapMapping(2*i+1).valid
          renamedRs2(i).bits := io.remapMapping(2*i+1).bits
        }
      }
    }
  }

  // Hook up instructions to ROB
  for (i <- 0 until 4) {
    io.robPorts(2*i) := renamedRs1(i).bits
    io.robPorts(2*i+1) := renamedRs2(i).bits
  }

  // Now, hook up the ouputs
  for (i <- 0 until 4) {
    // Valid bits for ROB: each valid instruction results in a valid ROB entry
    io.allocROB(i).valid := pipelinedInst(i).valid

    val robEntry = io.allocROB(i).bits // convenience

    // Which ROB entry
    robEntry.entry := renamedDest(i)

    // Bulk wire data from decoded instruction
    robEntry.op := pipelinedInst(i).bits.op
    robEntry.funct3 := pipelinedInst(i).bits.funct3
    when (pipelinedOpDecode(i).hasRs2) {
      robEntry.funct7 := pipelinedInst(i).bits.funct7
    } .otherwise {
      robEntry.funct7 := UInt(0, 7)
    }
    // First operand
    when (renamedRs1(i).valid) {
      // Getting from ROB
      robEntry.rs1Map := Bool(true)
      robEntry.rs1Rename := renamedRs1(i).bits
      robEntry.rs1Val.valid := io.robDest(2*i).valid
      robEntry.rs1Val.bits := io.robDest(2*i).bits
    } .otherwise {
      // Getting from Arch Reg File
      robEntry.rs1Map := Bool(false)
      robEntry.rs1Rename := pipelinedInst(i).bits.rs1
      robEntry.rs1Val.valid := Bool(true)
      robEntry.rs1Val.bits := io.rfValues(2*i)
    }

    // Second operand
    when (pipelinedOpDecode(i).hasRs2) {
      // Second operand is a register
      when (renamedRs2(i).valid) {
        // Getting from ROB
        robEntry.rs2Map := Bool(true)
        robEntry.rs2Rename := renamedRs2(i).bits
        robEntry.rs2Val.valid := io.robDest(2*i+1).valid
        robEntry.rs2Val.bits := io.robDest(2*i+1).bits
      } .otherwise {
        // Getting from Arch Reg File
        robEntry.rs2Map := Bool(false)
        robEntry.rs2Rename := pipelinedInst(i).bits.rs2
        robEntry.rs2Val.valid := Bool(true)
        robEntry.rs2Val.bits := io.rfValues(2*i+1)
      }
    } .otherwise {
      // Second operand is an immediate
      robEntry.rs2Map := Bool(false)
      robEntry.rs2Rename := UInt(0)
      robEntry.rs2Val.valid := Bool(true)
      robEntry.rs2Val.bits := UInt(0) // chisel requires this for some reason :(

      // Choose which immediate````````````````
      when (pipelinedOpDecode(i).hasImmI) {
        robEntry.rs2Val.bits := pipelinedInst(i).bits.immI
      } .elsewhen (pipelinedOpDecode(i).hasImmS) {
        robEntry.rs2Val.bits := pipelinedInst(i).bits.immS
      } .elsewhen (pipelinedOpDecode(i).hasImmB) {
        robEntry.rs2Val.bits := pipelinedInst(i).bits.immB
      } .elsewhen (pipelinedOpDecode(i).hasImmU) {
        robEntry.rs2Val.bits := pipelinedInst(i).bits.immU
      } .otherwise { // J Immediate
        robEntry.rs2Val.bits := pipelinedInst(i).bits.immJ
      }
    }

    // There are two ways we discussed to do the speculative bit:
    // 1) An instruction is speculative if either operand is speculative. This
    //    allows us to do optimizations with re-execution, but it is more
    //    complicated to reason about, so for now...
    //
    // 2) An instruction is speculative if last instruction was speculative or
    //    if this instruction is a jump. NOTE: A jump instruction is itself
    //    marked with a `speculative` bit, so when squashing instructions, we
    //    need to be careful not to squash the jump too.  If i = 0, speculative
    //    bit will come from ROB otherwise spec will come from last entry.
    //
    // NOTE: Loads can not be source of speculation as they are always after store, 
    // so currently loads are not source of speculation - TODO!
    if (i == 0) {
      when (pipelinedInst(i).bits.op === UInt(0x63)) {
        robEntry.spec := UInt(1) 
      } .otherwise {
      robEntry.spec := io.robSpec
      }
    } else {
      when (pipelinedInst(i).bits.op === UInt(0x63)) {
	robEntry.spec := UInt(1)
      } .otherwise {
	robEntry.spec := io.allocROB(i-1).bits.spec
      }
    }

    // Remap entries:
    // - a remap entry should be written if the instruction has a destination
    // register and no later instruction is renaming the same arch reg
    //
    // - the value of the remap entry is the ROB entry number
    io.allocRemap(i).valid := pipelinedOpDecode(i).hasRd &&
      pipelinedInst(i).valid &&
      io.allocRemap
        .slice(i+1, io.allocRemap.size)
        .foldLeft(Bool(true)) {
          // j is the next allocRemap entry being considered
          // i is valid if j is not or if j maps a different register
          (total, j) => total && (!j.valid || j.bits.reg != io.allocRemap(i).bits.reg)
        }
    io.allocRemap(i).bits.reg := pipelinedInst(i).bits.rd
    io.allocRemap(i).bits.idxROB := renamedDest(i)
  }
}

class RiscyAllocTests(c: RiscyAlloc) extends Tester(c) {
  // TEST 1
  // add r1 <- r1 + 0xFFFF
  // add r2 <- r1 + 0xFFFF
  
  poke(c.io.inst(0).valid, 1)
  poke(c.io.inst(0).bits.op, 0x13)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x1)
  poke(c.io.inst(0).bits.rd, 0x1)
  poke(c.io.inst(0).bits.immI, 0xFFFF)

  poke(c.io.inst(1).valid, 1)
  poke(c.io.inst(1).bits.op, 0x13)
  poke(c.io.inst(1).bits.funct3, 0x0)
  poke(c.io.inst(1).bits.rs1, 0x1)
  poke(c.io.inst(1).bits.rd, 0x2)
  poke(c.io.inst(1).bits.immI, 0xFFFF)

  poke(c.io.inst(2).valid, 0)
  poke(c.io.inst(3).valid, 0)
  
  poke(c.io.robFree, 64)
  poke(c.io.robFirst, 0)

  poke(c.io.robDest(2).valid, 0)
  poke(c.io.robDest(2).bits, 0x34)
  
  poke(c.io.rfValues(0), 0x1234)
  poke(c.io.robSpec, false)
  poke(c.io.remapMapping(0).valid, 0)
  poke(c.io.remapMapping(1).valid, 0)
  poke(c.io.remapMapping(2).valid, 0)
  poke(c.io.remapMapping(3).valid, 0)
  step(1)

  // TODO: expect output to ROB

  // Should map r1 to ROB0
  expect(c.io.allocRemap(0).valid, 1)
  expect(c.io.allocRemap(0).bits.reg, 1)
  expect(c.io.allocRemap(0).bits.idxROB, 0)
  // ROB entry for 1st Inst
  expect(c.io.allocROB(0).valid, 1)
  expect(c.io.allocROB(0).bits.op, 0x13)
  expect(c.io.allocROB(0).bits.funct3, 0x0)
  expect(c.io.allocROB(0).bits.funct7, 0x00)
  expect(c.io.allocROB(0).bits.rs1Map, false)
  expect(c.io.allocROB(0).bits.rs1Rename, 0x1)
  expect(c.io.allocROB(0).bits.rs1Val.valid, true)
  expect(c.io.allocROB(0).bits.rs1Val.bits, 0x1234)
  expect(c.io.allocROB(0).bits.rs2Map, false)
  expect(c.io.allocROB(0).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(0).bits.rs2Val.valid, true)
  expect(c.io.allocROB(0).bits.rs2Val.bits, 0xFFFF)
  expect(c.io.allocROB(0).bits.spec, 0)

  // Should map r2 to ROB1
  expect(c.io.allocRemap(1).valid, 1)
  expect(c.io.allocRemap(1).bits.reg, 2)
  expect(c.io.allocRemap(1).bits.idxROB, 1)
  // ROB entry for 2nd Inst
  expect(c.io.allocROB(1).valid, 1)
  expect(c.io.allocROB(1).valid, 1)
  expect(c.io.allocROB(1).bits.op, 0x13)
  expect(c.io.allocROB(1).bits.funct3, 0x0)
  expect(c.io.allocROB(1).bits.funct7, 0x00)
  expect(c.io.allocROB(1).bits.rs1Map, true)
  expect(c.io.allocROB(1).bits.rs1Rename, 0x00)
  expect(c.io.allocROB(1).bits.rs1Val.valid, false)
  expect(c.io.allocROB(1).bits.rs1Val.bits, 0x34)
  expect(c.io.allocROB(1).bits.rs2Map, false)
  expect(c.io.allocROB(1).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(1).bits.rs2Val.valid, true)
  expect(c.io.allocROB(1).bits.rs2Val.bits, 0xFFFF)
  expect(c.io.allocROB(1).bits.spec, 0)


  expect(c.io.allocRemap(2).valid, 0)
  expect(c.io.allocRemap(3).valid, 0)
  expect(c.io.allocROB(2).valid, 0)
  expect(c.io.allocROB(3).valid, 0)


  // TEST2
  // add r1 <- r1 + 0xFFFF
  // add r1 <- r1 + 0xFFFF
  // add r1 <- r1 + 0xFFFF
  // add r1 <- r1 + 0xFFFF

  poke(c.io.inst(0).valid, 1)
  poke(c.io.inst(0).bits.op, 0x13)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x1)
  poke(c.io.inst(0).bits.rd, 0x1)
  poke(c.io.inst(0).bits.immI, 0xFFFF)

  poke(c.io.inst(1).valid, 1)
  poke(c.io.inst(1).bits.op, 0x13)
  poke(c.io.inst(1).bits.funct3, 0x0)
  poke(c.io.inst(1).bits.rs1, 0x1)
  poke(c.io.inst(1).bits.rd, 0x1)
  poke(c.io.inst(1).bits.immI, 0xFFFF)

  poke(c.io.inst(2).valid, 1)
  poke(c.io.inst(2).bits.op, 0x13)
  poke(c.io.inst(2).bits.funct3, 0x0)
  poke(c.io.inst(2).bits.rs1, 0x1)
  poke(c.io.inst(2).bits.rd, 0x1)
  poke(c.io.inst(2).bits.immI, 0xFFFF)

  poke(c.io.inst(3).valid, 1)
  poke(c.io.inst(3).bits.op, 0x13)
  poke(c.io.inst(3).bits.funct3, 0x0)
  poke(c.io.inst(3).bits.rs1, 0x1)
  poke(c.io.inst(3).bits.rd, 0x1)
  poke(c.io.inst(3).bits.immI, 0xFFFF)
  
  poke(c.io.robFree, 62)
  poke(c.io.robFirst, 2)
  
  poke(c.io.robDest(0).valid, 0)
  poke(c.io.robDest(0).bits, 0x34)
  poke(c.io.robDest(2).valid, 0)
  poke(c.io.robDest(2).bits, 0x34)
  poke(c.io.robDest(4).valid, 0)
  poke(c.io.robDest(4).bits, 0x34)
  poke(c.io.robDest(6).valid, 0)
  poke(c.io.robDest(6).bits, 0x34)
  
  poke(c.io.rfValues(0), 0x1234)
  poke(c.io.robSpec, false)
  poke(c.io.remapMapping(0).valid, 1)
  poke(c.io.remapMapping(1).valid, 1)
  poke(c.io.remapMapping(2).valid, 1)
  poke(c.io.remapMapping(3).valid, 1)
 
  step(1)

  // TODO: expect output to ROB

  // Should map r1 to ROB5
  expect(c.io.allocRemap(3).valid, 1)
  expect(c.io.allocRemap(3).bits.reg, 1)
  expect(c.io.allocRemap(3).bits.idxROB, 5)

  expect(c.io.allocRemap(0).valid, 0)
  expect(c.io.allocRemap(1).valid, 0)
  expect(c.io.allocRemap(2).valid, 0)

  expect(c.io.allocROB(0).valid, 1)
  expect(c.io.allocROB(0).bits.op, 0x13)
  expect(c.io.allocROB(0).bits.funct3, 0x0)
  expect(c.io.allocROB(0).bits.funct7, 0x00)
  expect(c.io.allocROB(0).bits.rs1Map, true)
  expect(c.io.allocROB(0).bits.rs1Rename, 0x0)
  expect(c.io.allocROB(0).bits.rs1Val.valid, false)
  expect(c.io.allocROB(0).bits.rs1Val.bits, )
  expect(c.io.allocROB(0).bits.rs2Map, )
  expect(c.io.allocROB(0).bits.rs2Rename, )
  expect(c.io.allocROB(0).bits.rs2Val.valid, )
  expect(c.io.allocROB(0).bits.rs2Val.bits, )
  expect(c.io.allocROB(0).bits.spec, )

  expect(c.io.allocROB(1).valid, 1)
  expect(c.io.allocROB(1).bits.op, 0x13)
  expect(c.io.allocROB(1).bits.funct3, 0x0)
  expect(c.io.allocROB(1).bits.funct7, 0x00)
  expect(c.io.allocROB(1).bits.rs1Map, )
  expect(c.io.allocROB(1).bits.rs1Rename, )
  expect(c.io.allocROB(1).bits.rs1Val.valid, )
  expect(c.io.allocROB(1).bits.rs1Val.bits, )
  expect(c.io.allocROB(1).bits.rs2Map, )
  expect(c.io.allocROB(1).bits.rs2Rename, )
  expect(c.io.allocROB(1).bits.rs2Val.valid, )
  expect(c.io.allocROB(1).bits.rs2Val.bits, )
  expect(c.io.allocROB(1).bits.spec, )

  expect(c.io.allocROB(2).valid, 1)
  expect(c.io.allocROB(2).bits.op, 0x13)
  expect(c.io.allocROB(2).bits.funct3, 0x0)
  expect(c.io.allocROB(2).bits.funct7, 0x00)
  expect(c.io.allocROB(2).bits.rs1Map, )
  expect(c.io.allocROB(2).bits.rs1Rename, )
  expect(c.io.allocROB(2).bits.rs1Val.valid, )
  expect(c.io.allocROB(2).bits.rs1Val.bits, )
  expect(c.io.allocROB(2).bits.rs2Map, )
  expect(c.io.allocROB(2).bits.rs2Rename, )
  expect(c.io.allocROB(2).bits.rs2Val.valid, )
  expect(c.io.allocROB(2).bits.rs2Val.bits, )
  expect(c.io.allocROB(2).bits.spec, )

  expect(c.io.allocROB(3).valid, 1)
  expect(c.io.allocROB(3).bits.op, 0x13)
  expect(c.io.allocROB(3).bits.funct3, 0x0)
  expect(c.io.allocROB(3).bits.funct7, 0x00)
  expect(c.io.allocROB(3).bits.rs1Map, )
  expect(c.io.allocROB(3).bits.rs1Rename, )
  expect(c.io.allocROB(3).bits.rs1Val.valid, )
  expect(c.io.allocROB(3).bits.rs1Val.bits, )
  expect(c.io.allocROB(3).bits.rs2Map, )
  expect(c.io.allocROB(3).bits.rs2Rename, )
  expect(c.io.allocROB(3).bits.rs2Val.valid, )
  expect(c.io.allocROB(3).bits.rs2Val.bits, )
  expect(c.io.allocROB(3).bits.spec, )

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
  poke(c.io.inst(0).bits.op, 0x13)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x1)
  poke(c.io.inst(0).bits.rd, 0x1)
  poke(c.io.inst(0).bits.immI, 0xFFF)

  poke(c.io.inst(1).valid, 1)
  poke(c.io.inst(1).bits.op, 0x13)
  poke(c.io.inst(1).bits.funct3, 0x0)
  poke(c.io.inst(1).bits.rs1, 0x1)
  poke(c.io.inst(1).bits.rd, 0x2)
  poke(c.io.inst(1).bits.immI, 0xFFF)

  poke(c.io.inst(2).valid, 1)
  poke(c.io.inst(2).bits.op, 0x13)
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
  
//  expect(c.io.allocROB(1).valid, )
//  expect(c.io.allocROB(1).bits.op, )
//  expect(c.io.allocROB(1).bits.funct3, )
//  expect(c.io.allocROB(1).bits.funct7, )
//  expect(c.io.allocROB(1).bits.rs1Map, )
//  expect(c.io.allocROB(1).bits.rs1Rename, )
//  expect(c.io.allocROB(1).bits.rs1Val.valid, )
//  expect(c.io.allocROB(1).bits.rs1Val.bits, )
//  expect(c.io.allocROB(1).bits.rs2Map, )
//  expect(c.io.allocROB(1).bits.rs2Rename, )
//  expect(c.io.allocROB(1).bits.rs2Val.valid, )
//  expect(c.io.allocROB(1).bits.rs2Val.bits, )
//  expect(c.io.allocROB(1).bits.spec, )
//
//  expect(c.io.allocROB(2).valid, )
//  expect(c.io.allocROB(2).bits.op, )
//  expect(c.io.allocROB(2).bits.funct3, )
//  expect(c.io.allocROB(2).bits.funct7, )
//  expect(c.io.allocROB(2).bits.rs1Map, )
//  expect(c.io.allocROB(2).bits.rs1Rename, )
//  expect(c.io.allocROB(2).bits.rs1Val.valid, )
//  expect(c.io.allocROB(2).bits.rs1Val.bits, )
//  expect(c.io.allocROB(2).bits.rs2Map, )
//  expect(c.io.allocROB(2).bits.rs2Rename, )
//  expect(c.io.allocROB(2).bits.rs2Val.valid, )
//  expect(c.io.allocROB(2).bits.rs2Val.bits, )
//  expect(c.io.allocROB(2).bits.spec, )

  expect(c.io.allocRemap(1).valid, 0)
  expect(c.io.allocRemap(3).valid, 0)

  // TODO: add more tests

  // add r4 <- r3 + r5
  // add r5 <- r4 + r6
  // add r6 <- r5 + r7

  poke(c.io.inst(0).valid, 1)
  poke(c.io.inst(0).bits.op, 0x33)
  poke(c.io.inst(0).bits.funct3, 0x0)
  poke(c.io.inst(0).bits.rs1, 0x3)
  poke(c.io.inst(0).bits.rd, 0x4)
  poke(c.io.inst(0).bits.rs2, 0x5)

  poke(c.io.inst(1).valid, 1)
  poke(c.io.inst(1).bits.op, 0x33)
  poke(c.io.inst(1).bits.funct3, 0x0)
  poke(c.io.inst(1).bits.rs1, 0x4)
  poke(c.io.inst(1).bits.rd, 0x5)
  poke(c.io.inst(1).bits.immI, 0x6)

  poke(c.io.inst(2).valid, 1)
  poke(c.io.inst(2).bits.op, 0x33)
  poke(c.io.inst(2).bits.funct3, 0x0)
  poke(c.io.inst(2).bits.rs1, 0x5)
  poke(c.io.inst(2).bits.rd, 0x6)
  poke(c.io.inst(2).bits.immI, 0x7)

  poke(c.io.inst(3).valid, 0)

  poke(c.io.robFree, 55)
  poke(c.io.robFirst, 9)

  step(1)



}

class AllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyAllocTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
}
