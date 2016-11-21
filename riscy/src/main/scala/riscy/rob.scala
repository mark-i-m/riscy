package riscy

import Chisel._

// An extended version of the decoded instruction with the same
// signals as DecodeIns and also the renamed register names and
// any available values.
class ROBEntry extends DecodeIns {
  // Renamed registers (not valid if reg is in arch regfile)
  val rs1Rename = UInt(OUTPUT, 6)
  val rs2Rename = UInt(OUTPUT, 6)

  // Available operands with ready bits
  val rs1Val = Valid(UInt(OUTPUT, 64))
  val rs2Val = Valid(UInt(OUTPUT, 64))

  // Destination register with ready bit
  val rdVal = Valid(UInt(OUTPUT, 64))

  // Unique instruction tag (ROB entry number)
  val pc = UInt(OUTPUT, 64)
  val tag = UInt(OUTPUT, 6)

  // From OpDecode:
  // - does this instruction have a dest reg
  // - is this instruction a jump
  val hasRd = Bool(OUTPUT)
  val isSt = Bool(OUTPUT)
  val isLd = Bool(OUTPUT)

  // From BP:
  // - was this branch predicted taken? 1 => T, 0 => NT
  val predTaken = Bool(OUTPUT)
  val isMispredicted = Bool(OUTPUT)
}

class WBValue extends Bundle {
  // Which physical reg?
  val id = Valid(UInt(INPUT, 6))
  // The value to write back
  val value = UInt(INPUT, 32)
  // Is this a taken branch
  val taken = Bool(INPUT)
}

class ROB extends Module {
  val io = new Bundle {
    // Get signals from allocate
    //  - remap table
    //  - RF
    //  - ROB
    val remapPorts = Vec.fill(8) { UInt(INPUT, 5) }
    val remapMapping = Vec.fill(8) { Valid(UInt(OUTPUT, 6)).asOutput }

    val rfPorts = Vec.fill(8) { UInt(INPUT, 5) }
    val rfValues = Vec.fill(8) { UInt(OUTPUT, 64) }

    val robPorts = Vec.fill(8) { UInt(INPUT, 6) }
    val robDest = Vec.fill(8) { Valid(UInt(OUTPUT, 64)).asOutput }
    val robFree = UInt(OUTPUT, 7) // How many free entries
    val robFirst = UInt(OUTPUT, 6) // Index of the first free entry

    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()).flip }
    val allocROB = Vec.fill(4) { Valid(new ROBEntry()).flip }

    // Get signals from the FOO for WB
    // - ALU 0-3
    // - LSQ 0, 1
    val wbValues = Vec.fill(6) { (new WBValue).asInput }

    // Signal to load/store to actually issue a store.
    // There is exactly one store that could be at the head of the LSQ, so we
    // only need to indicate when to actually write to memory.
    val stCommit = Vec.fill(4) { Bool(OUTPUT) }

    // Signal that a misprediction occured.
    //
    // mispredPC will be the address of the mispredicted branch. It will be
    // valid if there was a misprediction and invalid otherwise.
    //
    // mispredTarget is the correct target of the mispredicted branch.
    val mispredPC = Valid(UInt(OUTPUT, 64))
    val mispredTarget = UInt(OUTPUT, 64)

    // Should ROB produce a stall?
    val robStallReq = Bool(OUTPUT)

    // Should ROB consume a stall?
    // TODO: Need to stop
    // - stCommit
    // - mispredPC
    // - mispredTarget
    //
    // Don't need to stall
    // - anything in commit except under the above circumstances
    val robStall = Bool(INPUT)
  }

  // The register remap table
  // Bit 0 of each register denotes if that register is in the Arch register
  // (1) or in the ROB (0).
  //
  // The remaining bits denote which ROB entry if the register is in the ROB
  //
  // NOTE: writing regs is idempotent, so we need no special handling here for
  // stalls as long as we continue to present the same input signals :)
  val remap = Module(new RegFile(32, 12, 8, i => Valid(UInt(width = 6)), 
    "remap", (x: ValidIO[UInt]) => x.bits))

  // The Architectural register file
  val rf = Module(new RegFile(32, 8, 4, i => UInt(width = 64), "RF"))

  // The ROB storage structure
  val robW = Vec.fill(64) { Valid(new ROBEntry) } // Write enable and write value for ROB
  val rob = Vec.tabulate(64) { i => RegEnable(robW(i).bits, robW(i).valid) }

  // most senior
  val head = new MultiCounter(64)
  val headInc = UInt()

  // # free entries
  var tailInc = UInt()
  var freeInc = UInt()
  val free = Reg(init = UInt(64), next = freeInc)

  when(io.mispredPC.valid) {
    freeInc := UInt(64)
    head.reset
  } .otherwise {
    freeInc := free + headInc - tailInc
    head.inc(headInc)
  }

  io.robStallReq := free < UInt(4)

  /////////////////////////////////////////////////////////////////////////////
  // Allocation logic
  /////////////////////////////////////////////////////////////////////////////
  io.robFirst := head.value + (UInt(64) - free)
  io.robFree  := free

  for(i <- 0 until 8) {
    remap.io.rPorts(i) := io.remapPorts(i)
    io.remapMapping(i) := remap.io.rValues(i)

    rf.io.rPorts(i) := io.rfPorts(i)
    io.rfValues(i) := rf.io.rValues(i)

    io.robDest(i) := rob(io.robPorts(i)).rdVal
  }

  when(!io.robStallReq) {
    // Latch new remap table mappings
    for(i <- 0 until 4) {
      remap.io.wPorts(i).valid  := io.allocRemap(i).valid
      remap.io.wPorts(i).bits   := io.allocRemap(i).bits.reg
      remap.io.wValues(i).valid := Bool(true)
      remap.io.wValues(i).bits  := io.allocRemap(i).bits.idxROB
    }

    // Latch new ROB entries
    for(i <- 0 until 64) {
      when(UInt(i) === io.robFirst) {
        robW(i) := io.allocROB(0)
        when(io.allocROB(0).valid) {
          printf("Latch new ins, ROB%d PC: %x\n", UInt(i), io.allocROB(0).bits.pc)
        }
      } .elsewhen(UInt(i) === io.robFirst + UInt(1)) {
        robW(i) := io.allocROB(1)
        when(io.allocROB(1).valid) {
          printf("Latch new ins, ROB%d PC: %x\n", UInt(i), io.allocROB(1).bits.pc)
        }
      } .elsewhen(UInt(i) === io.robFirst + UInt(2)) {
        robW(i) := io.allocROB(2)
        when(io.allocROB(2).valid) {
          printf("Latch new ins, ROB%d PC: %x\n", UInt(i), io.allocROB(2).bits.pc)
        }
      } .elsewhen(UInt(i) === io.robFirst + UInt(3)) {
        robW(i) := io.allocROB(3)
        when(io.allocROB(3).valid) {
          printf("Latch new ins, ROB%d PC: %x\n", UInt(i), io.allocROB(3).bits.pc)
        }
      } .otherwise {
        robW(i).valid := Bool(false) // write disable
        robW(i).bits  := new ROBEntry
      }
    }
  } .otherwise {
    // Don't latch new remap table mappings
    for(i <- 0 until 4) {
      remap.io.wPorts(i).valid  := Bool(false)
      remap.io.wPorts(i).bits   := UInt(0)
      remap.io.wValues(i).valid := Bool(false)
      remap.io.wValues(i).bits  := UInt(0)
    }

    // Don't latch new ROB entries
    for(i <- 0 until 64) {
      robW(i).valid := Bool(false) // write disable
      robW(i).bits  := new ROBEntry
    }
  }

  // Update tail pointer
  tailInc := PopCount(Array.tabulate(4) { io.allocROB(_).valid })

  /////////////////////////////////////////////////////////////////////////////
  // Writeback logic
  /////////////////////////////////////////////////////////////////////////////
  // TODO: How to ignore flushed instructions? In all likelihood, the ROB entry
  // number will not match, so they will be ignored, but this case is not
  // guaranteed, so how to handle?
  for(i <- 0 until 6) {
    when(io.wbValues(i).id.valid) {
      robW(io.wbValues(i).id.bits).valid := Bool(true)
      // The ROB entry stays the same, but the rdVal changes
      robW(io.wbValues(i).id.bits).bits := rob(io.wbValues(i).id.bits)
      robW(io.wbValues(i).id.bits).bits.rdVal.valid := Bool(true)
      robW(io.wbValues(i).id.bits).bits.rdVal.bits := io.wbValues(i).value
      robW(io.wbValues(i).id.bits).bits.isMispredicted :=
        io.wbValues(i).taken ^ robW(io.wbValues(i).id.bits).bits.predTaken

      printf("writing WB value to ROB%d: %x\n", io.wbValues(i).id.bits, io.wbValues(i).value)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Commit logic
  /////////////////////////////////////////////////////////////////////////////
  // - 4-wide in order commit
  // - Only commit if the head of the queue has a ready destination bit
  // - Stores should actually make their memory requests at this time
  // - Instructions with an rd should actually write the Arch reg file
  // - Branches that were mispredicted should trigger cleanup at this point
  //   (TODO: can be done earlier if we regenerate the remap table from the ROB)
  // - Move the head pointer
  // - Clear popped entries by changing rdVal.valid to false
  // - Clear mappings in the remap table on commit
  // - Clear mappings in the remap table on mispredict
  /////////////////////////////////////////////////////////////////////////////

  // For convenience, create label wires for the four instructions at the head
  // of ROB.
  //
  // front(i) = head + i
  val front = Vec.tabulate(4) { i => rob(head.value + UInt(i)) }

  // Compute a simple flag that tells if an instruction could commit this cycle.
  // This should only happen if it is at the front of the queue (first 4) AND
  // all the instructions before it are ready to commit AND the ready bit for its
  // destination is set AND the previous instruction is not a mispredicted branch.
  //
  // couldCommit(i) corresponds to front(i)
  val couldCommit = Vec.fill(4) { Bool() }
  for(i <- 0 until 4) {
    couldCommit(i) :=
      (if(i > 0) { couldCommit(i-1) } else { Bool(true) }) &&
      front(i).rdVal.valid &&
      (if(i > 0) { !front(i-1).isMispredicted } else { Bool(true) })

    when(couldCommit(i)) {
      printf("Commit ROB%d\n", head.value + UInt(i))
    }
  }

  // If the head of the ROB is a store, tell the L/SQ to actually write to
  // memory now that the store has committed.
  for(i <- 0 until 4) {
    io.stCommit(i) := rob(head.value + UInt(i)).isSt && couldCommit(i)
  }

  // If the head of the ROB is a mispredicted branch...
  val mispredFlags = Vec.tabulate(4) {
    i => couldCommit(i) && front(i).isMispredicted
  }
  val firstMispred: UInt = PriorityEncoder(mispredFlags)

  io.mispredPC.valid := mispredFlags.exists(identity _)
  io.mispredPC.bits  := MuxLookup(firstMispred, UInt(0),
    Array.tabulate(4) { i => UInt(i) -> front(i).pc})
  io.mispredTarget   := MuxLookup(firstMispred, UInt(0),
    Array.tabulate(4) { 
      i => UInt(i) -> Mux(front(i).isMispredicted ^ front(i).predTaken, 
        front(i).rdVal.bits, front(i).pc + UInt(4))
    })

  // Clear remap table on misprediction
  remap.io.reset.valid := io.mispredPC.valid
  remap.io.reset.bits.valid := Bool(false)
  remap.io.reset.bits.bits  := UInt(0)

  when(io.mispredPC.valid) {
    printf("MISPREDICT\n")
  }

  // Write to the register file
  //
  // - For each destination register among the four committing instructions,
  //   find out which is the last to rename that register.
  // - If this instruction is the last to rename, then write to RF
  val lastToRename = Vec.tabulate(4) { i =>
    PriorityMux(Array.tabulate(4) { j =>
      (front(i).rd === front(j).rd && front(j).hasRd && couldCommit(j), UInt(j))
    })
  }

  for(i <- 0 until 4) {
    // Want to only write the last value out of all ready instructions.
    // i.e. if multiple committing instructions want to write the same reg,
    // only the last write should win...
    rf.io.wPorts(i).valid := couldCommit(i) &&
                             front(i).hasRd &&
                             lastToRename(i) === UInt(i)
    rf.io.wPorts(i).bits := front(i).rd
    rf.io.wValues(i)     := front(i).rdVal.bits

    when(rf.io.wPorts(i).valid) {
      printf("latch to rf committing ROB%d: %x\n", head.value + UInt(i), front(i).rdVal.bits)
    } .elsewhen(couldCommit(i) && !lastToRename(i) === UInt(i) && front(i).hasRd) {
      printf("not latching to rf front(%d), ROB%d, r%d: %x\n",
        UInt(i), head.value + UInt(i), front(i).rd, front(i).rdVal.bits)
      printf("\tlastToRename(0) = %d\n", lastToRename(0))
      printf("\tlastToRename(1) = %d\n", lastToRename(1))
      printf("\tlastToRename(2) = %d\n", lastToRename(2))
      printf("\tlastToRename(3) = %d\n", lastToRename(3))
    }
  }

  // Clear remap table entries
  //
  // An entry should be cleared if
  // - that register was mapped to an instruction that is committing
  // - that register is not being remapped by an incoming instruction
  val shouldUnmap = Vec.fill(4) { Valid(UInt(width = 5)) }
  for (i <- 0 until 4) {
    // Is an incoming instruction mapping the destination
    val rdReRemapped = (Vec.tabulate(4) {j =>
      io.allocROB(j).valid &&
      io.allocROB(j).bits.rd === front(i).rd &&
      io.allocROB(j).bits.hasRd
    }).exists(identity[Bool] _)

    shouldUnmap(i).valid := couldCommit(i) &&
                            remap.io.rValues(i+8).valid &&
                            remap.io.rValues(i+8).bits === head.value + UInt(i) &&
                            !rdReRemapped
    shouldUnmap(i).bits  := front(i).rd
  }

  for(i <- 4 until 8) {
    remap.io.rPorts(i+4) := front(i-4).rd

    remap.io.wPorts(i).valid := shouldUnmap(i-4).valid
    remap.io.wPorts(i).bits  := shouldUnmap(i-4).bits
    remap.io.wValues(i).valid := Bool(false)
    remap.io.wValues(i).bits := UInt(0)
  }

  // Clear ROB entries
  for(i <- 0 until 64) {
    // On commit
    when(UInt(i) === head.value && couldCommit(0)) {
      robW(i).valid := Bool(true)
      robW(i).bits := new ROBEntry
    } .elsewhen(UInt(i) === head.value + UInt(1) && couldCommit(1)) {
      robW(i).valid := Bool(true)
      robW(i).bits := new ROBEntry
    } .elsewhen(UInt(i) === head.value + UInt(2) && couldCommit(2)) {
      robW(i).valid := Bool(true)
      robW(i).bits := new ROBEntry
    } .elsewhen(UInt(i) === head.value + UInt(3) && couldCommit(3)) {
      robW(i).valid := Bool(true)
      robW(i).bits := new ROBEntry
    }

    // Clear all entries on mispredict
    when(io.mispredPC.valid) {
      robW(i).valid := Bool(true)
      robW(i).bits := new ROBEntry
    }
  }

  // Compute the new head
  headInc := PopCount(couldCommit)
}

class ROBTests(c: ROB) extends Tester(c) {
  def pokeRemapPorts(remap: Array[(Int, Int)]) =
    remap foreach { x => poke(c.io.remapPorts(x._1), x._2) }

  def expectRemapMappingValid(remap: Array[(Int, Boolean)]) =
    remap foreach { x => expect(c.io.remapMapping(x._1).valid, x._2) }

  def expectRemapMappingBits(remap: Array[(Int, Int)]) =
    remap foreach { x => expect(c.io.remapMapping(x._1).bits, x._2) }

  def pokeRemapping(port: Int, reg: Int, rob: Int) = {
    poke(c.io.allocRemap(port).valid, true)
    poke(c.io.allocRemap(port).bits.reg, reg)
    poke(c.io.allocRemap(port).bits.idxROB, rob)
  }

  def pokeRemappingInvalid(ports: Array[Int]) =
    ports foreach { port => poke(c.io.allocRemap(port).valid, false) }

  // Add Register Immediate
  // if rs1Rename._1 => get value from ROB
  // else            => get value from RF
  //
  def pokeROBAddRI(port: Int, tag: Int, pc: Int,
    rs1: Int, rs1Rename: (Boolean, Int), rs1Val: (Boolean, Int),
    imm: Int, rd: Int) = {

    poke(c.io.allocROB(port).valid, true)
    poke(c.io.allocROB(port).bits.pc, pc)
    poke(c.io.allocROB(port).bits.tag, tag)
    poke(c.io.allocROB(port).bits.op, 0x13)
    poke(c.io.allocROB(port).bits.funct3, 0x0)
    poke(c.io.allocROB(port).bits.rs1, rs1)
    poke(c.io.allocROB(port).bits.rd, rd)
    poke(c.io.allocROB(port).bits.immI, imm)
    poke(c.io.allocROB(port).bits.hasRd, true)
    poke(c.io.allocROB(port).bits.isSt, false)
    poke(c.io.allocROB(port).bits.predTaken, false)
    poke(c.io.allocROB(port).bits.isMispredicted, false)
    poke(c.io.allocROB(port).bits.rs1Rename, if(rs1Rename._1) { rs1Rename._2 } else { 0 })
    poke(c.io.allocROB(port).bits.rs2Rename, 0) // Not renamed
    poke(c.io.allocROB(port).bits.rs1Val.valid, rs1Val._1) // Ready or ROB?
    poke(c.io.allocROB(port).bits.rs1Val.bits, if(rs1Val._1) { rs1Val._2 } else { 0 }) // Value from RF or ROB
    poke(c.io.allocROB(port).bits.rs2Val.valid, true) // Ready
    poke(c.io.allocROB(port).bits.rs2Val.bits, imm) // No RS2, use Imm
    poke(c.io.allocROB(port).bits.rdVal.valid, false) // NOT Ready
  }

  def pokeROBInvalid(ports: Array[Int]) =
    ports foreach { port => poke(c.io.allocROB(port).valid, false) }

  def pokeROBPorts(rob: Array[(Int, Int)]) =
    rob foreach { x => poke(c.io.robPorts(x._1), x._2) }

  def expectROBDestValid(rob: Array[(Int, Boolean)]) =
    rob foreach { x => expect(c.io.robDest(x._1).valid, x._2) }

  def expectROBDestBits(rob: Array[(Int, Int)]) =
    rob foreach { x => expect(c.io.robDest(x._1).bits, x._2) }

  def pokeWB(port: Int, id: Int, value: Int, taken: Boolean = false) = {
    poke(c.io.wbValues(port).id.valid, true)
    poke(c.io.wbValues(port).id.bits, id)
    poke(c.io.wbValues(port).value, value)
    poke(c.io.wbValues(port).taken, taken)
  }

  def pokeWBInvalid(ports: Array[Int]) =
    ports foreach { port => poke(c.io.wbValues(port).id.valid, false) }

  def expectStCommit(st: Array[Boolean]) =
    for(i <- 0 until 4) { expect(c.io.stCommit(i), st(i)) }

  // test the remap ports

  // Get the mapping for the first few registers and check that they are not
  // mapped to any physical register.
  pokeRemapPorts(Array.tabulate(8){ i => i -> i })

  step(0)

  expectRemapMappingValid(Array.tabulate(8){ i => i -> false })

  // map r0 to p4, port 0
  pokeRemapping(0, 0, 4)

  // map r1 to p3, port 1
  pokeRemapping(1, 1, 3)

  step(1)

  expectRemapMappingValid(
    Array.tabulate(2){ i => i -> true } ++
    Array.tabulate(6){ i => (i+2) -> false }
  )

  expectRemapMappingBits(
    Array(0 -> 4, 1 -> 3)
  )

  pokeRemappingInvalid(Array(0, 1))

  step(1)

  // expect the mapping not to have changed
  expectRemapMappingValid(
    Array.tabulate(2){ i => i -> true } ++
    Array.tabulate(6){ i => (i+2) -> false }
  )

  expectRemapMappingBits(
    Array(0 -> 4, 1 -> 3)
  )

  // map r0 to p7, port 0
  pokeRemapping(0, 0, 7)

  // map r30 to p8, port 1
  pokeRemapping(1, 30, 8)
  pokeRemapPorts(Array(2 -> 30))

  step(1)

  expectRemapMappingValid(
    Array.tabulate(3){ i => i -> true } ++
    Array.tabulate(5){ i => (i+3) -> false }
  )

  expectRemapMappingBits(
    Array(0 -> 7, 1 -> 3, 2 -> 8)
  )

  // Try some random reads and writes
  for(i <- 0 until 100) {
    val randRPort = rnd.nextInt(8)
    val randWPort = rnd.nextInt(4)
    val randWReg = rnd.nextInt(32)
    val randWROBIdx = rnd.nextInt(64)

    pokeRemappingInvalid(
      (Array.tabulate(4)(identity _)).filter(x => x != randWPort)
    )

    // map randWReg to randWROBIdx, port randWPort
    pokeRemapping(randWPort, randWReg, randWROBIdx)

    poke(c.io.remapPorts(randRPort), randWReg)

    step(1)

    expect(c.io.remapMapping(randRPort).valid, true)
    expect(c.io.remapMapping(randRPort).bits, randWROBIdx)
  }

  // Try latching some instructions
  //
  // The rest of this test executes a simple program
  //
  // 0   add r1 <- r1 + 0xFFFF
  // 1   add r2 <- r1 + 0xFFFF
  //
  // 2   add r1 <- r1 + 0xFFFF
  // 3   add r1 <- r1 + 0xFFFF
  // 4   add r1 <- r1 + 0xFFFF
  // 5   add r1 <- r1 + 0xFFFF
  //
  // 6   lw r3 <- 0x100[r1]
  //
  // 7   bne r1 != r2 => PC + 0x44
  // 8   sw -0x100[r2] <- r3
  //
  // 9   beq r1 == r2 => PC - 0x44
  // A   sw -0x100[r2] <- r3
  // B   r1 <- r1 + 0xFFFF

  // Beginning of tests...

  // add r1 <- r1 + 0xFFFF
  pokeROBAddRI(0, 0, 0xa0, 1, (false, 0), (true, 0xDEADBEEF), 0xFFFF, 1)

  // map r1 to ROB0, port 0
  pokeRemapping(0, 1, 0)

  // add r2 <- r1 + 0xFFFF
  pokeROBAddRI(1, 1, 0xa4, 1, (true, 0), (false, 0), 0xFFFF, 2)

  // map r2 to ROB 1, port 1
  pokeRemapping(1, 2, 1)

  pokeRemappingInvalid(Array(2, 3))

  expect(c.io.robFree, 64)
  expect(c.io.robFirst, 0)

  step(1)

  pokeRemapPorts(Array(0 -> 1, 1 -> 2))

  pokeROBPorts(Array.tabulate(6){i => i -> i})

  step(0)

  // expect proper rob size and tail entry
  expect(c.io.robFree, 62)
  expect(c.io.robFirst, 2)

  expectRemapMappingValid(Array(0 -> true, 1 -> true))
  expectRemapMappingBits(Array(0 -> 0, 1 -> 1))

  // expect that result are not ready
  expectROBDestValid(Array(0 -> false, 1 -> false))

  // add r1 <- r1 + 0xFFFF
  pokeROBAddRI(0, 2, 0xa8, 1, (true, 0), (false, 0), 0xFFFF, 1)

  // add r1 <- r1 + 0xFFFF
  pokeROBAddRI(1, 3, 0xac, 1, (true, 2), (false, 0), 0xFFFF, 1)

  // add r1 <- r1 + 0xFFFF
  pokeROBAddRI(2, 4, 0xb0, 1, (true, 3), (false, 0), 0xFFFF, 1)

  // add r1 <- r1 + 0xFFFF
  pokeROBAddRI(3, 5, 0xb4, 1, (true, 4), (false, 0), 0xFFFF, 1)

  // remap r1 to ROB5, port 3
  pokeRemapping(3, 1, 5)

  pokeRemappingInvalid(Array(0, 1))

  step(1)

  expect(c.io.robFree, 58)
  expect(c.io.robFirst, 6)

  expectRemapMappingValid(Array(0 -> true, 1 -> true))
  expectRemapMappingBits(Array(0 -> 5, 1 -> 1))

  expectROBDestValid(Array.tabulate(6){ i => i -> false }) // Rds not ready

  // Test writeback

  pokeROBInvalid(Array.tabulate(4)(identity _))
  pokeRemappingInvalid(Array(3))

  // result of first add comes back
  pokeWB(0, 0, 0xDEAEBEEE) // 0xDEADBEEF + 0xFFFF

  step(1)

  expect(c.io.robFree, 58)
  expect(c.io.robFirst, 6)

  expectRemapMappingValid(Array(0 -> true, 1 -> true))
  expectRemapMappingBits(Array(0 -> 5, 1 -> 1))

  // ROB0 is ready
  expectROBDestValid(Array(0 -> true))
  expectROBDestBits(Array(0 -> 0xDEAEBEEE))

  // result of next two instructions come back
  pokeWB(0, 1, 0xDEAFBEED) // 0xDEAEBEEE + 0xFFFF
  pokeWB(1, 2, 0xDEAFBEED) // 0xDEAEBEEE + 0xFFFF

  step(1)

  // Also, committed the first instruction
  poke(c.io.rfPorts(0), 1) // Read r1

  expectRemapMappingValid(Array(0 -> true, 1 -> true))
  expectRemapMappingBits(Array(0 -> 5, 1 -> 1))

  // ROB0 committed
  // ROB1, 2 are ready
  expectROBDestValid(Array(1 -> true, 2 -> true, 0 -> false))
  expectROBDestBits(Array(1 -> 0xDEAFBEED, 2 -> 0xDEAFBEED))

  expect(c.io.robFree, 59)
  expect(c.io.robFirst, 6)

  // RF should have been written
  expect(c.io.rfValues(0), 0xDEAEBEEE)

  pokeWBInvalid(Array(0, 1))

  step(1)

  // Two more instructions commit
  poke(c.io.rfPorts(1), 2) // Read r2

  expectRemapMappingValid(Array(0 -> true, 1 -> false))
  expectRemapMappingBits(Array(0 -> 5))

  // ROB0-2 already committed
  expectROBDestValid(Array(1 -> false, 2 -> false, 0 -> false))

  expect(c.io.robFree, 61)
  expect(c.io.robFirst, 6)

  // RF should have been written
  expect(c.io.rfValues(0), 0xDEAFBEED)
  expect(c.io.rfValues(1), 0xDEAFBEED)

  // Try some other types of instructions now

  // lw (load word) r3 <- 0x100[r1]
  poke(c.io.allocROB(0).valid, true)
  poke(c.io.allocROB(0).bits.pc, 0xb8)
  poke(c.io.allocROB(0).bits.tag, 0x6)
  poke(c.io.allocROB(0).bits.op, 0x3)
  poke(c.io.allocROB(0).bits.funct3, 0x2) // word load, not byte, or double
  poke(c.io.allocROB(0).bits.rs1, 1)
  poke(c.io.allocROB(0).bits.rd, 3)
  poke(c.io.allocROB(0).bits.immI, 0x100)
  poke(c.io.allocROB(0).bits.hasRd, true)
  poke(c.io.allocROB(0).bits.isSt, false)
  poke(c.io.allocROB(0).bits.predTaken, false)
  poke(c.io.allocROB(0).bits.isMispredicted, false)
  poke(c.io.allocROB(0).bits.rs1Rename, 5) // Renamed
  poke(c.io.allocROB(0).bits.rs2Rename, 0) // Not renamed
  poke(c.io.allocROB(0).bits.rs1Val.valid, false) // Ready
  poke(c.io.allocROB(0).bits.rs1Val.bits, 0) // Value from RF
  poke(c.io.allocROB(0).bits.rs2Val.valid, true) // Ready
  poke(c.io.allocROB(0).bits.rs2Val.bits, 0x100) // No RS2, use Imm
  poke(c.io.allocROB(0).bits.rdVal.valid, false) // NOT Ready

  // remap r3 to ROB6, port 0
  pokeRemapping(0, 3, 6)
  pokeRemapPorts(Array(3 -> 3))

  // write back ROB3
  pokeWB(0, 3, 0xDEB0BEEC) // 0xDEAFBEED + 0xFFFF

  step(1)

  pokeROBInvalid(Array(0))
  pokeRemappingInvalid(Array(0))

  expect(c.io.robFree, 60)
  expect(c.io.robFirst, 7)

  expectRemapMappingValid(Array(0 -> true, 3 -> true))
  expectRemapMappingBits(Array(0 -> 5, 3 -> 6))

  expectROBDestValid(Array(3 -> true, 6 -> false))
  expectROBDestBits(Array(3 -> 0xDEB0BEEC))

  // write back ROB4
  pokeWB(0, 4, 0xDEB1BEEB) // 0xDEB0BEEC + 0xFFFF

  step(1)

  expect(c.io.robFree, 61)
  expect(c.io.robFirst, 7)

  expectRemapMappingValid(Array(0 -> true, 3 -> true))
  expectRemapMappingBits(Array(0 -> 5, 3 -> 6))

  // ROB3 committed
  expectROBDestValid(Array(3 -> false, 4-> true, 6 -> false))
  expectROBDestBits(Array(4 -> 0xDEB1BEEB))

  // RF should have been written
  expect(c.io.rfValues(0), 0xDEB0BEEC)

  // write back ROB5
  pokeWB(0, 5, 0xDEB2BEEA) // 0xDEB1BEEB + 0xFFFF

  // bne r1 != r2 => PC + 0x44
  poke(c.io.allocROB(0).valid, true)
  poke(c.io.allocROB(0).bits.pc, 0xbc)
  poke(c.io.allocROB(0).bits.tag, 0x7)
  poke(c.io.allocROB(0).bits.op, 0x63)
  poke(c.io.allocROB(0).bits.funct3, 0x1) // not equal
  poke(c.io.allocROB(0).bits.rs1, 1)
  poke(c.io.allocROB(0).bits.rs2, 2)
  poke(c.io.allocROB(0).bits.rd, 0) // No rd
  poke(c.io.allocROB(0).bits.immB, 0x44)
  poke(c.io.allocROB(0).bits.hasRd, false)
  poke(c.io.allocROB(0).bits.isSt, false)
  poke(c.io.allocROB(0).bits.predTaken, true)
  poke(c.io.allocROB(0).bits.isMispredicted, false) // Don't know yet
  poke(c.io.allocROB(0).bits.rs1Rename, 5) // Renamed
  poke(c.io.allocROB(0).bits.rs2Rename, 0) // Not renamed
  poke(c.io.allocROB(0).bits.rs1Val.valid, false) // Not Ready
  poke(c.io.allocROB(0).bits.rs1Val.bits, 0) // Value from ROB
  poke(c.io.allocROB(0).bits.rs2Val.valid, true) // Ready
  poke(c.io.allocROB(0).bits.rs2Val.bits, 0xDEAEBEEE) // R2 from RF
  poke(c.io.allocROB(0).bits.rdVal.valid, false) // NOT Ready

  // sw (store word) -0x100[r2] <- r3
  poke(c.io.allocROB(1).valid, true)
  poke(c.io.allocROB(1).bits.pc, 0x100)
  poke(c.io.allocROB(1).bits.tag, 0x8)
  poke(c.io.allocROB(1).bits.op, 0x23)
  poke(c.io.allocROB(1).bits.funct3, 0x2) // word, not byte, or double
  poke(c.io.allocROB(1).bits.rs1, 2)
  poke(c.io.allocROB(1).bits.rs2, 3)
  poke(c.io.allocROB(1).bits.rd, 0) // No dest
  poke(c.io.allocROB(1).bits.immS, -0x100)
  poke(c.io.allocROB(1).bits.hasRd, false)
  poke(c.io.allocROB(1).bits.isSt, true)
  poke(c.io.allocROB(1).bits.predTaken, false)
  poke(c.io.allocROB(1).bits.isMispredicted, false)
  poke(c.io.allocROB(1).bits.rs1Rename, 0) // Not renamed
  poke(c.io.allocROB(1).bits.rs2Rename, 6) // Renamed
  poke(c.io.allocROB(1).bits.rs1Val.valid, true) // Ready
  poke(c.io.allocROB(1).bits.rs1Val.bits, 0xDEAEBEEE) // Value from RF
  poke(c.io.allocROB(1).bits.rs2Val.valid, false) // Not Ready
  poke(c.io.allocROB(1).bits.rs2Val.bits, 0) // Value from ROB
  poke(c.io.allocROB(1).bits.rdVal.valid, false) // NOT Ready

  step(1)

  expect(c.io.robFree, 60)
  expect(c.io.robFirst, 9)

  expectRemapMappingValid(Array(0 -> true, 3 -> true))
  expectRemapMappingBits(Array(0 -> 5, 3 -> 6))

  // ROB4 committed
  expectROBDestValid(Array(4-> false, 5-> true, 6 -> false))
  expectROBDestBits(Array(5 -> 0xDEB2BEEA))

  // RF should have been written
  expect(c.io.rfValues(0), 0xDEB1BEEB)

  // beq r1 == r2 => PC - 0x44
  poke(c.io.allocROB(0).valid, true)
  poke(c.io.allocROB(0).bits.pc, 0x104)
  poke(c.io.allocROB(0).bits.tag, 0x9)
  poke(c.io.allocROB(0).bits.op, 0x63)
  poke(c.io.allocROB(0).bits.funct3, 0x0) // equal
  poke(c.io.allocROB(0).bits.rs1, 1)
  poke(c.io.allocROB(0).bits.rs2, 2)
  poke(c.io.allocROB(0).bits.rd, 0) // No rd
  poke(c.io.allocROB(0).bits.immB, -0x44)
  poke(c.io.allocROB(0).bits.hasRd, false)
  poke(c.io.allocROB(0).bits.isSt, false)
  poke(c.io.allocROB(0).bits.predTaken, true)
  poke(c.io.allocROB(0).bits.isMispredicted, false) // Don't know yet
  poke(c.io.allocROB(0).bits.rs1Rename, 5) // Renamed
  poke(c.io.allocROB(0).bits.rs2Rename, 0) // Not renamed
  poke(c.io.allocROB(0).bits.rs1Val.valid, false) // Not Ready
  poke(c.io.allocROB(0).bits.rs1Val.bits, 0) // Value from ROB
  poke(c.io.allocROB(0).bits.rs2Val.valid, true) // Ready
  poke(c.io.allocROB(0).bits.rs2Val.bits, 0xDEAFBEED) // R2 from RF
  poke(c.io.allocROB(0).bits.rdVal.valid, false) // NOT Ready

  // sw (store word) -0x100[r2] <- r3
  poke(c.io.allocROB(1).valid, true)
  poke(c.io.allocROB(1).bits.pc, 0xbc)
  poke(c.io.allocROB(1).bits.tag, 0xa)
  poke(c.io.allocROB(1).bits.op, 0x23)
  poke(c.io.allocROB(1).bits.funct3, 0x2) // word, not byte, or double
  poke(c.io.allocROB(1).bits.rs1, 2)
  poke(c.io.allocROB(1).bits.rs2, 3)
  poke(c.io.allocROB(1).bits.rd, 0) // No dest
  poke(c.io.allocROB(1).bits.immS, -0x100)
  poke(c.io.allocROB(1).bits.hasRd, false)
  poke(c.io.allocROB(1).bits.isSt, true)
  poke(c.io.allocROB(1).bits.predTaken, false)
  poke(c.io.allocROB(1).bits.isMispredicted, false)
  poke(c.io.allocROB(1).bits.rs1Rename, 0) // Not renamed
  poke(c.io.allocROB(1).bits.rs2Rename, 6) // Renamed
  poke(c.io.allocROB(1).bits.rs1Val.valid, true) // Ready
  poke(c.io.allocROB(1).bits.rs1Val.bits, 0xDEAFBEED) // Value from RF
  poke(c.io.allocROB(1).bits.rs2Val.valid, false) // Not Ready
  poke(c.io.allocROB(1).bits.rs2Val.bits, 0) // Value from ROB
  poke(c.io.allocROB(1).bits.rdVal.valid, false) // NOT Ready

  // r1 <- r1 + 0xFFFF
  pokeROBAddRI(2, 0xb, 0xc0, 1, (true, 5), (true, 0xDEB2BEEA), 0xFFFF, 1)
  pokeRemapping(2, 1, 0xb)

  // NOTE: lw address is ready, begin cache access

  step(1)

  pokeROBInvalid(Array(0, 1, 2))
  pokeRemappingInvalid(Array(2))
  pokeWBInvalid(Array(0))

  expect(c.io.robFree, 58)
  expect(c.io.robFirst, 0xc)

  expectRemapMappingValid(Array(0 -> true, 3 -> true))
  expectRemapMappingBits(Array(0 -> 0xb, 3 -> 6))

  // ROB5 committed
  expectROBDestValid(Array(5-> false, 6 -> false))

  // RF should have been written
  expect(c.io.rfValues(0), 0xDEB2BEEA)

  // NOTE: bne executes OoO in this cycle
  // NOTE: beq executes OoO in this cycle
  // NOTE: add executes OoO in this cycle

  step(1)

  expect(c.io.robFree, 58)
  expect(c.io.robFirst, 0xc)

  expectRemapMappingValid(Array(0 -> true, 3 -> true))
  expectRemapMappingBits(Array(0 -> 0xb, 3 -> 6))

  expectROBDestValid(Array.tabulate(7) { i => i -> false })

  // load writes back
  pokeWB(0, 6, 0x5687)

  // bne writes back
  pokeWB(1, 7, 0x100, true)

  // beq writes back
  pokeWB(2, 9, 0xbc, false)

  // add writes back
  pokeWB(3, 0xb, 0xDEB3BEE9)

  step(1)

  pokeWBInvalid(Array(0,1,2,3))

  expect(c.io.robFree, 58)
  expect(c.io.robFirst, 0xc)

  expectRemapMappingValid(Array(0 -> true, 3 -> true))
  expectRemapMappingBits(Array(0 -> 0xb, 3 -> 6))

  pokeROBPorts(Array.tabulate(8) { i => i -> (i + 5) })

  expectROBDestValid(Array(0-> false, 1 -> true, 2 -> true, 4 -> true, 6 -> true))
  expectROBDestBits(Array(1 -> 0x5687, 2 -> 0x100, 4 -> 0xbc, 6 -> 0xDEB3BEE9))

  // check that second store does not commit
  expectStCommit(Array.fill(4)(false))

  // NOTE: load commits
  // NOTE: bne commits

  // NOTE: first sw begins address computation OoO this cycle
  // NOTE: second sw begins address computation OoO this cycle

  step(1)

  // ROB 6-7 commmitted
  expect(c.io.robFree, 60)
  expect(c.io.robFirst, 0xc)

  expectRemapMappingValid(Array(0 -> true, 3 -> false))
  expectRemapMappingBits(Array(0 -> 0xb))

  expectROBDestValid(Array(1 -> false, 2 -> false, 4 -> true, 6 -> true))
  expectROBDestBits(Array(4 -> 0xbc, 6 -> 0xDEB3BEE9))

  poke(c.io.rfPorts(3), 3) // Read r3
  expect(c.io.rfValues(3), 0x5687)

  // first sw writesback
  pokeWB(4, 8, 0xDEAFBDED)

  // second sw writesback
  pokeWB(5, 0xa, 0xDEAFBDED)

  // check that second store does not commit
  expectStCommit(Array.fill(4)(false))

  step(1)

  pokeWBInvalid(Array(0,1,2,3,4,5))

  expect(c.io.robFree, 60)
  expect(c.io.robFirst, 0xc)

  expectRemapMappingValid(Array(0 -> true))
  expectRemapMappingBits(Array(0 -> 0xb))

  expectROBDestValid(Array(3 -> true, 4 -> true, 5 -> true, 6 -> true))
  expectROBDestBits(Array(3 -> 0xDEAFBDED, 4 -> 0xbc, 5 -> 0xDEAFBDED, 6 -> 0xDEB3BEE9))

  // NOTE: first sw commits
  // NOTE: beq commits... oh no! mispredicted!
  // NOTE: second sw and add are squashed
  // NOTE: remap table is squashed

  expect(c.io.mispredPC.valid, true)
  expect(c.io.mispredPC.bits, 0x104)
  expect(c.io.mispredTarget, 0x108)

  // check that first store commits
  expectStCommit(Array(true, false, false, false))

  step(1)

  // Mispredict -> squash all instructions
  expect(c.io.robFree, 64)
  expect(c.io.robFirst, 0x0)

  expectRemapMappingValid(Array(0 -> false))

  expectROBDestValid(Array(4 -> false, 5 -> false, 6 -> false, 7 -> false))

  // check that second store does not commit
  expectStCommit(Array.fill(4)(false))

  step(1)

  // No state changes
  expect(c.io.robFree, 64)
  expect(c.io.robFirst, 0x0)

  expectRemapMappingValid(Array(0 -> false))

  expectROBDestValid(Array(4 -> false, 5 -> false, 6 -> false, 7 -> false))

  // Check that RF is correct
  
  poke(c.io.rfPorts(0), 1) // Read r1
  poke(c.io.rfPorts(1), 2) // Read r2
  poke(c.io.rfPorts(2), 3) // Read r3
  expect(c.io.rfValues(0), 0xDEB2BEEA)
  expect(c.io.rfValues(1), 0xDEAFBEED)
  expect(c.io.rfValues(2), 0x5687)

  // check that second store does not commit
  expectStCommit(Array.fill(4)(false))
}

class ROBGenerator extends TestGenerator {
  def genMod(): Module = Module(new ROB())
  def genTest[T <: Module](c: T): Tester[T] =
    (new ROBTests(c.asInstanceOf[ROB])).asInstanceOf[Tester[T]]
}
