package riscy

import Chisel._

// Information from the Allocate/Rename stage to the Remap table.
class AllocRemap extends Bundle {
  val reg = UInt(OUTPUT, 5) // which reg to rename
  val idxROB = UInt(OUTPUT, 6) // ROB entry number to map to
}

// The Control Magic to rename instructions and send signals to update the
// Remap table, the ROB, and the Issue Queues.
class RiscyAlloc extends Module {
  val io = new Bundle {
    // Input from the rotator and decode logic with 4 decoded instructions
    val inst = Vec.fill(4) { Valid(new DecodeIns()).flip }
    val pc = Vec.fill(4) {UInt(INPUT, 64)}

    // Access the Remap table to find out what the current mappings are (so we
    // can rename)
    val remapPorts = Vec.fill(8) { UInt(OUTPUT, 5) }
    val remapMapping = Vec.fill(8) { Valid(UInt(INPUT, 6)).asInput }

    // Arch register access is needed to get reg value for ROB entry
    val rfPorts = Vec.fill(8) { UInt(OUTPUT, 5) }
    val rfValues = Vec.fill(8) { UInt(INPUT, 64) }

    // ROB table access to populate next ROB entry
    val robPorts = Vec.fill(8) { UInt(OUTPUT, 6) }
    val robDest = Vec.fill(8) { Valid(UInt(INPUT, 64)).asInput }
    val robFree = UInt(INPUT, 6) // How many free entries TODO: do we even need this? -MM
    val robFirst = UInt(INPUT, 6) // Index of the first free entry

    // Outputs to the Remap table and the ROB with the correct values to update
    // for this cycle. 
    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()) }
    val allocROB = Vec.fill(4) { Valid(new ROBEntry()) }

    // Should alloc stall?
    val allocStall = Bool(INPUT)

		// Era for misspeculation handling
		val allocEra = UInt(INPUT, 7)
  }

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
    i => RegEnable(opDecodes(i).io.opInfo, !io.allocStall)
  }

  val pipelinedInst = Vec.tabulate(4) {
    i => RegEnable(io.inst(i), !io.allocStall)
  }

  val pipelinedPc = Vec.tabulate(4) {
    i => RegEnable(io.pc(i), !io.allocStall)
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
  val renamedRs1 = Vec.tabulate(4) { i => 
    PriorityMux(
      ((Array.tabulate(i) { j => (
        pipelinedInst(j).bits.rd === pipelinedInst(i).bits.rs1,
        {
          val renamed = Valid(UInt(6))
          renamed.valid := pipelinedOpDecode(j).hasRd 
          renamed.bits  := renamedDest(j)
          renamed
        }
      )}).reverse :+ (Bool(true), io.remapMapping(2*i)))
    ) 
  }
  val renamedRs2 = Vec.tabulate(4) { i => 
    PriorityMux(
      ((Array.tabulate(i) { j => (
        pipelinedInst(j).bits.rd === pipelinedInst(i).bits.rs2,
        {
          val renamed = Valid(UInt(6))
          renamed.valid := pipelinedOpDecode(j).hasRd 
          renamed.bits  := renamedDest(j)
          renamed
        }
      )}).reverse :+ (Bool(true), io.remapMapping(2*i+1)))
    ) 
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

    // Operation
    robEntry.tag := renamedDest(i)
    robEntry.pc := pipelinedPc(i)
    robEntry.op := pipelinedInst(i).bits.op
    robEntry.funct3 := pipelinedInst(i).bits.funct3
    robEntry.funct7 := Mux(pipelinedOpDecode(i).hasRs2, pipelinedInst(i).bits.funct7, UInt(0, 7))
    robEntry.isSt := pipelinedOpDecode(i).isSt
    robEntry.isLd := pipelinedOpDecode(i).isLd
    robEntry.isHalt := pipelinedOpDecode(i).isHalt
    robEntry.hasRd := pipelinedOpDecode(i).hasRd
		robEntry.era := io.allocEra
    
		// Destination
    robEntry.rd := pipelinedInst(i).bits.rd

    // Immediates
    robEntry.immI := pipelinedInst(i).bits.immI
    robEntry.immS := pipelinedInst(i).bits.immS
    robEntry.immB := pipelinedInst(i).bits.immB
    robEntry.immU := pipelinedInst(i).bits.immU
    robEntry.immJ := pipelinedInst(i).bits.immJ

    // First operand
    when (pipelinedOpDecode(i).isHalt) {
      // If this is a halt, do not rename instructions
      robEntry.rs1Rename := UInt(0)
      robEntry.rs1Val.valid := Bool(true)
      robEntry.rs1Val.bits := UInt(0)
    } .elsewhen (renamedRs1(i).valid) {
      // Getting from ROB
      //robEntry.rs1Map := Bool(true)
      robEntry.rs1Rename := renamedRs1(i).bits
      robEntry.rs1Val.valid := io.robDest(2*i).valid
      robEntry.rs1Val.bits := io.robDest(2*i).bits
    } .otherwise {
      // Getting from Arch Reg File
      //robEntry.rs1Map := Bool(false)
      robEntry.rs1Rename := pipelinedInst(i).bits.rs1
      robEntry.rs1Val.valid := Bool(true)
      robEntry.rs1Val.bits := io.rfValues(2*i)
    }

    // Second operand
    when(pipelinedOpDecode(i).isHalt) {
      // If this is a halt, do not rename instructions
      robEntry.rs2Rename := UInt(0)
      robEntry.rs2Val.valid := Bool(true)
      robEntry.rs2Val.bits := UInt(0)
    } .elsewhen (pipelinedOpDecode(i).hasRs2) {
      // Second operand is a register
      when (renamedRs2(i).valid) {
        // Getting from ROB
        //robEntry.rs2Map := Bool(true)
        robEntry.rs2Rename := renamedRs2(i).bits
        robEntry.rs2Val.valid := io.robDest(2*i+1).valid
        robEntry.rs2Val.bits := io.robDest(2*i+1).bits
      } .otherwise {
        // Getting from Arch Reg File
        //robEntry.rs2Map := Bool(false)
        robEntry.rs2Rename := pipelinedInst(i).bits.rs2
        robEntry.rs2Val.valid := Bool(true)
        robEntry.rs2Val.bits := io.rfValues(2*i+1)
      }
    } .otherwise {
      // Second operand is an immediate
      //robEntry.rs2Map := Bool(false)
      robEntry.rs2Rename := UInt(0)
      robEntry.rs2Val.valid := Bool(true)
      robEntry.rs2Val.bits := UInt(0) // chisel requires this for some reason :(

      // Choose which immediate
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
  poke(c.io.remapMapping(0).valid, 0)
  poke(c.io.remapMapping(1).valid, 0)
  poke(c.io.remapMapping(2).valid, 0)
  poke(c.io.remapMapping(3).valid, 0)
  step(1)

  // Control
  // TODO expect pc
  expect(c.io.allocROB(0).bits.tag, 0)
  expect(c.io.allocROB(0).bits.hasRd, true)
  expect(c.io.allocROB(0).bits.isSt, false)
  expect(c.io.allocROB(0).bits.isLd, false)
  expect(c.io.allocROB(0).bits.isHalt, false)
  // Should map r1 to ROB0
  expect(c.io.allocRemap(0).valid, 1)
  expect(c.io.allocRemap(0).bits.reg, 1)
  expect(c.io.allocRemap(0).bits.idxROB, 0)
  // ROB entry for 1st Inst
  expect(c.io.allocROB(0).valid, 1)
  expect(c.io.allocROB(0).bits.op, 0x13)
  expect(c.io.allocROB(0).bits.funct3, 0x0)
  expect(c.io.allocROB(0).bits.funct7, 0x00)
  //expect(c.io.allocROB(0).bits.rs1Map, false)
  expect(c.io.allocROB(0).bits.rs1Rename, 0x1)
  expect(c.io.allocROB(0).bits.rs1Val.valid, true)
  expect(c.io.allocROB(0).bits.rs1Val.bits, 0x1234)
  //expect(c.io.allocROB(0).bits.rs2Map, false)
  expect(c.io.allocROB(0).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(0).bits.rs2Val.valid, true)
  expect(c.io.allocROB(0).bits.rs2Val.bits, 0xFFFF)


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
  //expect(c.io.allocROB(1).bits.rs1Map, true)
  expect(c.io.allocROB(1).bits.rs1Rename, 0x00)
  expect(c.io.allocROB(1).bits.rs1Val.valid, false)
  expect(c.io.allocROB(1).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(1).bits.rs2Map, false)
  expect(c.io.allocROB(1).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(1).bits.rs2Val.valid, true)
  expect(c.io.allocROB(1).bits.rs2Val.bits, 0xFFFF)


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
  
  poke(c.io.remapMapping(0).bits, 0)
  poke(c.io.remapMapping(0).valid, 1)
  poke(c.io.remapMapping(2).valid, 1)
  poke(c.io.remapMapping(4).valid, 1)
  poke(c.io.remapMapping(6).valid, 1)
 
  step(1)

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
  //expect(c.io.allocROB(0).bits.rs1Map, true)
  expect(c.io.allocROB(0).bits.rs1Rename, 0x0)
  expect(c.io.allocROB(0).bits.rs1Val.valid, false)
  expect(c.io.allocROB(0).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(0).bits.rs2Map, false)
  expect(c.io.allocROB(0).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(0).bits.rs2Val.valid, true)
  expect(c.io.allocROB(0).bits.rs2Val.bits, 0xFFFF)
  
  expect(c.io.allocROB(1).valid, 1)
  expect(c.io.allocROB(1).bits.op, 0x13)
  expect(c.io.allocROB(1).bits.funct3, 0x0)
  expect(c.io.allocROB(1).bits.funct7, 0x00)
  //expect(c.io.allocROB(1).bits.rs1Map, true)
  expect(c.io.allocROB(1).bits.rs1Rename, 0x2)
  expect(c.io.allocROB(1).bits.rs1Val.valid, false)
  expect(c.io.allocROB(1).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(1).bits.rs2Map, false)
  expect(c.io.allocROB(1).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(1).bits.rs2Val.valid, true)
  expect(c.io.allocROB(1).bits.rs2Val.bits, 0xFFFF)

  expect(c.io.allocROB(2).valid, 1)
  expect(c.io.allocROB(2).bits.op, 0x13)
  expect(c.io.allocROB(2).bits.funct3, 0x0)
  expect(c.io.allocROB(2).bits.funct7, 0x00)
  //expect(c.io.allocROB(2).bits.rs1Map, true)
  expect(c.io.allocROB(2).bits.rs1Rename, 0x3)
  expect(c.io.allocROB(2).bits.rs1Val.valid, false)
  expect(c.io.allocROB(2).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(2).bits.rs2Map, false)
  expect(c.io.allocROB(2).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(2).bits.rs2Val.valid, true)
  expect(c.io.allocROB(2).bits.rs2Val.bits, 0xFFFF)

  expect(c.io.allocROB(3).valid, 1)
  expect(c.io.allocROB(3).bits.op, 0x13)
  expect(c.io.allocROB(3).bits.funct3, 0x0)
  expect(c.io.allocROB(3).bits.funct7, 0x00)
  //expect(c.io.allocROB(3).bits.rs1Map, true)
  expect(c.io.allocROB(3).bits.rs1Rename, 0x4)
  expect(c.io.allocROB(3).bits.rs1Val.valid, false)
  expect(c.io.allocROB(3).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(3).bits.rs2Map, false)
  expect(c.io.allocROB(3).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(3).bits.rs2Val.valid, true)
  expect(c.io.allocROB(3).bits.rs2Val.bits, 0xFFFF)

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
  expect(c.io.allocROB(0).valid, 0)
  expect(c.io.allocROB(1).valid, 0)
  expect(c.io.allocROB(2).valid, 0)
  expect(c.io.allocROB(3).valid, 0)

  // TEST 4
  // add r1 <- r1 + 0xFFFF
  // add r2 <- r1 + 0xFFFF
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

  poke(c.io.inst(2).valid, 1)
  poke(c.io.inst(2).bits.op, 0x13)
  poke(c.io.inst(2).bits.funct3, 0x0)
  poke(c.io.inst(2).bits.rs1, 0x1)
  poke(c.io.inst(2).bits.rd, 0x2)
  poke(c.io.inst(2).bits.immI, 0xFFFF)

  poke(c.io.inst(3).valid, 0)
  
  poke(c.io.robFree, 58)
  poke(c.io.robFirst, 6)

  poke(c.io.robDest(0).valid, 0)
  poke(c.io.robDest(0).bits, 0x34)
  poke(c.io.robDest(2).valid, 0)
  poke(c.io.robDest(2).bits, 0x34)
  poke(c.io.robDest(4).valid, 0)
  poke(c.io.robDest(4).bits, 0x34)
  
  poke(c.io.remapMapping(0).bits, 5)
  poke(c.io.remapMapping(0).valid, 1)
  poke(c.io.remapMapping(2).valid, 1)
  poke(c.io.remapMapping(4).valid, 1)

  step(1)

  // Should map r1 to ROB6
  expect(c.io.allocRemap(0).valid, 1)
  expect(c.io.allocRemap(0).bits.reg, 1)
  expect(c.io.allocRemap(0).bits.idxROB, 6)

  // Should map r2 to ROB8
  expect(c.io.allocRemap(2).valid, 1)
  expect(c.io.allocRemap(2).bits.reg, 2)
  expect(c.io.allocRemap(2).bits.idxROB, 8)
  
  expect(c.io.allocROB(0).valid, 1)
  expect(c.io.allocROB(0).bits.op, 0x13)
  expect(c.io.allocROB(0).bits.funct3, 0x0)
  expect(c.io.allocROB(0).bits.funct7, 0x00)
  //expect(c.io.allocROB(0).bits.rs1Map, true)
  expect(c.io.allocROB(0).bits.rs1Rename, 0x5)
  expect(c.io.allocROB(0).bits.rs1Val.valid, false)
  expect(c.io.allocROB(0).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(0).bits.rs2Map, false)
  expect(c.io.allocROB(0).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(0).bits.rs2Val.valid, true)
  expect(c.io.allocROB(0).bits.rs2Val.bits, 0xFFFF)
  
  expect(c.io.allocROB(1).valid, 1)
  expect(c.io.allocROB(1).bits.op, 0x13)
  expect(c.io.allocROB(1).bits.funct3, 0x0)
  expect(c.io.allocROB(1).bits.funct7, 0x00)
  //expect(c.io.allocROB(1).bits.rs1Map, true)
  expect(c.io.allocROB(1).bits.rs1Rename, 0x6)
  expect(c.io.allocROB(1).bits.rs1Val.valid, false)
  expect(c.io.allocROB(1).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(1).bits.rs2Map, false)
  expect(c.io.allocROB(1).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(1).bits.rs2Val.valid, true)
  expect(c.io.allocROB(1).bits.rs2Val.bits, 0xFFFF)

  expect(c.io.allocROB(2).valid, 1)
  expect(c.io.allocROB(2).bits.op, 0x13)
  expect(c.io.allocROB(2).bits.funct3, 0x0)
  expect(c.io.allocROB(2).bits.funct7, 0x00)
  //expect(c.io.allocROB(2).bits.rs1Map, true)
  expect(c.io.allocROB(2).bits.rs1Rename, 0x6)
  expect(c.io.allocROB(2).bits.rs1Val.valid, false)
  expect(c.io.allocROB(2).bits.rs1Val.bits, 0x34)
  //expect(c.io.allocROB(2).bits.rs2Map, false)
  expect(c.io.allocROB(2).bits.rs2Rename, 0x0)
  expect(c.io.allocROB(2).bits.rs2Val.valid, true)
  expect(c.io.allocROB(2).bits.rs2Val.bits, 0xFFFF)


  expect(c.io.allocRemap(1).valid, 0)
  expect(c.io.allocRemap(3).valid, 0)
  expect(c.io.allocROB(3).valid, 0)

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
  
  poke(c.io.remapMapping(0).valid, 0)
  poke(c.io.remapMapping(1).valid, 0)
  poke(c.io.remapMapping(2).valid, 0)
  poke(c.io.remapMapping(3).valid, 0)
  poke(c.io.remapMapping(4).valid, 0)
  poke(c.io.remapMapping(5).valid, 0)

  poke(c.io.rfValues(0), 0x1111)
  poke(c.io.rfValues(1), 0x2222)
  poke(c.io.rfValues(2), 0x3333)
  poke(c.io.rfValues(3), 0x4444)
  poke(c.io.rfValues(4), 0x5555)
  poke(c.io.rfValues(5), 0x6666)
  step(1)
}

class AllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyAllocTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
}
