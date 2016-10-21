package riscy

import Chisel._

// Information from the Allocate/Rename stage to the Remap table.
class AllocRemap extends Bundle {
  val reg = UInt(OUTPUT, 5) // which reg to rename
  val idxROB = UInt(OUTPUT, 6) // ROB entry number to map to
}

// Information from the Allocate/Rename stage to the ROB and IQs.
class AllocROB extends Bundle {
  val entry = UInt(OUTPUT, 6) // which ROB entry to latch
  val inst = UInt(OUTPUT,118) // the renamed instruction
}

// The Control Magic to rename instructions and send signals to update the
// Remap table, the ROB, and the Issue Queues.
class RiscyAlloc extends Module {
  val io = new Bundle {
    // Input from the rotator and decode logic with 4 decoded instructions
    val inst = Vec.fill(4) { Valid(new DecodeIns()).flip }

    // Inputs with the first free entry and the number of free entries in the ROB
    val freeROB = UInt(INPUT, 6)
    val firstROB = UInt(INPUT, 6)

    // Input from the Remap table to find out what the current mappings are (so
    // we can rename)
    val remapTable = Vec.fill(32) { Valid(UInt(INPUT, 6)) }

    // Outputs to the Remap table and the ROB with the correct values to update
    // for this cycle. Note that they might not all be valid.
    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()) }
    val allocROB = Vec.fill(4) { Valid(new AllocROB()) }

    // Arch register access is needed to get reg value for ROB entry
    val archReg = Vec.fill(32) { UInt(INPUT, 32) }

    // ROB table access to populate next ROB entry
    val ROBtable = Vec.fill(64) { UInt(INPUT, 118) }
  }

  // TODO: will need some latches to break this into two stages

  // For each instruction, determine what resources/registers it needs.
  val opDecodes = Array.tabulate(4) {
    i => {
      val od = Module(new RiscyOpDecode())
      od.io.op := io.inst(i).bits.op
      od
    }
  }

  // TODO: stall if ROB is full

  // Do a simple addition to rename the instructions. Every instruction gets
  // an ROB entry, regardless of how many registers it reads or writes. The
  // valid bits of the instructions and the number of free ROB entries determines
  // whether we should stall and how many entries we should put into the ROB.
  val renamedDest = Vec.tabulate(4) { i => io.firstROB + UInt(i, 2) }

  // Compute all possible renamings... for each instruction i and register r,
  // let p_{r,i} be the physical register name of register r as seen by
  // instruction i. Let dest(i) denote the physical register destination of
  // instruction i. Then, we know that
  //
  //      p_{r,0} = Mapping from remap table
  //      p_{r,i} = / dest(i-1)       if i-1 renamed r
  //                \ p_{r, i-1}      otherwise
  //
  // Note that this definition already accounts for renaming by i-2.
  var allRenamed: Array[Array[ValidIO[UInt]]] = Array()
  for (i <- 0 until 4) {
     allRenamed +:= Array.tabulate(32) { r =>
      if (i == 0) {
        // from remap table if i = 0
        io.remapTable(r)
      } else {
        // from i - 1 else
        val renamed = Valid(UInt(width = 6))
        when (io.inst(i-1).bits.rd === UInt(r) && opDecodes(i-1).io.opInfo.hasRd) {
          renamed.valid := Bool(true)
          renamed.bits := renamedDest(i-1)
        } .otherwise {
          renamed := allRenamed(i-1)(r)
        }
        renamed
      }
    }
  }

  // Now, hook up the ouputs
  for (i <- 0 until 4) {
    // Valid bits for ROB: each valid instruction results in a valid ROB entry
    // TODO: finish coding for 4 issue width with a way of keeping track of renaming table in mult-issue
    io.allocROB(i).valid      := io.inst(i).valid 
    io.allocROB(i).bits.entry := renamedDest[i]
    val instOp                := io.inst[i].bits.op            				// 7  bits
    val instFunct7            := io.inst[i].bits.funct7					// 7  bits
    val instFunct3            := io.inst[i].bits.funct3					// 3  bits
    val instRdValue 	      := UInt("h0000",32)					// 32 bits
    when (io.remaptable[io.inst[i].bits.rs1].valid) {
    val instRs1Map            := UInt(0)						// 1  bits
    val instRs1Number         := allRenamed[i][io.inst[i].bits.rs1]			// 6  bits
    val instRs1Value          := io.ROBtable[allRenamed[i][io.inst[i].bits.rs1]](56, 87)// 32 bits
    val instRs1Ready          := io.ROBtable[allRenamed[i][io.inst[i].bits.rs1]](130)	// 1  bits - rethink this, do we need to grep ready bit from ROB
    val instRs1Speculative    := io.ROBtable[allRenamed[i][io.inst[i].bits.rs1]](129) 	// 0  bits
    } .otherwise {
    val instRs1Map            := UInt(0)
    val instRs1Number	      := Cat(UInt(0), io.inst[i].bits.rs1)	
    val instRs1Value          := io.archReg[io.inst[i].bits.rs1]
    val instRs1Ready          := UInt(1)
    val instRs1Speculative    := UInt(0)
    }
    when (opDecodes[i].hasRs2) {
    when (io.remaptable[io.inst[i].bits.rs2].valid) {
    val instRs2ImmMap         := UInt(1)						// 1  bits
    val instRs2ImmNumber      := allRenamed[i][io.inst[i].bits.rs2]			// 6  bits
    val instRs2ImmValue       := io.ROBtable[allRenamed[i][io.inst[i].bits.rs2]](56,87) // 32 bits
    val instRs2ImmReady       := io.ROBtable[allRenamed[i][io.inst[i].bits.rs2]](130)	// 1  bits - rethink this, do we need to grep ready bit from ROB
    val instRs2ImmSpeculative := io.ROBtable[allRenamed[i][io.inst[i].bits.rs2]](129) 	// 0  bits
    } .otherwise {
    val instRs2ImmMap         := UInt(0)
    val instRs2ImmNumber      := Cat(UInt(0), io.inst[i].bits.rs2) 			// As ROB has 6 bit address to each reg, arch reg entry will be appended by a 0	
    val instRs2ImmValue       := io.archReg[io.inst[i].bits.rs2]
    val instRs2ImmReady       := UInt(1)
    val instRs2ImmSpeculative := UInt(0)
    } 
    } .elsewhen (opDecodes[i].hadImmI) {
    val instRs2ImmMap         := UInt(0)
    val instRs2ImmNumber      := UInt("h00",6)	
    val instRs2ImmValue       := io.archReg[io.inst[i].bits.immI]
    val instRs2ImmReady       := UInt(1)
    val instRs2ImmSpeculative := UInt(0)
    } .elsewhen (opDecodes[i].hadImmS) {
    val instRs2ImmMap         := UInt(0)
    val instRs2ImmNumber      := UInt("h00",6)	
    val instRs2ImmValue       := io.archReg[io.inst[i].bits.immS]
    val instRs2ImmReady       := UInt(1)
    val instRs2ImmSpeculative := UInt(0)
    } .elsewhen (opDecodes[i].hadImmB) {
    val instRs2ImmMap         := UInt(0)
    val instRs2ImmNumber      := UInt("h00",6)	
    val instRs2ImmValue       := io.archReg[io.inst[i].bits.immB]
    val instRs2ImmReady       := UInt(1)
    val instRs2ImmSpeculative := UInt(0)
    } .elsewhen (opDecodes[i].hadImmU) {
    val instRs2ImmMap         := UInt(0)
    val instRs2ImmNumber      := UInt("h00",6)	
    val instRs2ImmValue       := io.archReg[io.inst[i].bits.immU]
    val instRs2ImmReady       := UInt(1)
    val instRs2ImmSpeculative := UInt(0)
    } .elsewhen (opDecodes[i].hadImmJ) {
    val instRs2ImmMap         := UInt(0)
    val instRs2ImmNumber      := UInt("h00",6)	
    val instRs2ImmValue       := io.archReg[io.inst[i].bits.immJ]
    val instRs2ImmReady       := UInt(1)
    val instRs2ImmSpeculative := UInt(0)
    }
    val instSpeculative       := instRs1Speculative || instRs2ImmSpeculative		// 1 bits
    val instReady             := UInt(0)						// 1 bits - Ready bit is set only when instruction is commited

    io.allocROB(i).inst = Cat(instReady, instSpeculative, instRs2ImmMap, instRs2ImmNumber, instRs2ImmValue, instRs2ImmReady, instRs1Map, instRs1Number, instRs1Value, instRs1Ready, instRdValue, instFunct7, instFunct3, instOp)

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

  poke(c.io.freeROB, 64)
  poke(c.io.firstROB, 0)

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

  poke(c.io.freeROB, 62)
  poke(c.io.firstROB, 2)

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

  poke(c.io.freeROB, 58)
  poke(c.io.firstROB, 6)

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
