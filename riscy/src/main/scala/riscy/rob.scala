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

  // Unique instruction tag
  // TODO: for now, it is just the ROB entry number...
  val tag = UInt(OUTPUT, 6)

  // Speculative bit
  val spec = Bool(OUTPUT)
}

class ROB extends Module {
  val io = new Bundle {
    // TODO: what is the interface with writeback?

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
  }

  // The register remap table
  // Bit 0 of each register denotes if that register is in the Arch register
  // (1) or in the ROB (0).
  //
  // The remaining bits denote which ROB entry if the register is in the ROB
  val remap = Module(new RegFile(32, 8, 4, i => Valid(UInt(width = 6))))

  // The ROB storage structure
  val rob = Vec.fill(64) { new ROBEntry() }

  // The Architectural register file
  val rf = Module(new RegFile(32, 8, 4, i => UInt(width = 64)))





  // Commit logic
  // - 4-wide in order commit
  // - Only commit if the head of the queue has a ready destination bit
  // - Stores should actually make their memory requests at this time
  // - Instructions with an rd should actually write the Arch reg file
  // - Branches that were mispredicted should trigger cleanup at this point
  //   (TODO: possibly can be done earlier?)
  // - TODO: move the head pointer
  
  // For convenience, create label wires for the four instructions at the head
  // of ROB.
  //
  // front(i) = head + i
  val front = Vec.tabulate(4) { rob(head + _) }

  // Compute a simple flag that tells if an instruction could commit this cycle.
  // This should only happen if it is at the front of the queue (first 4) AND
  // all the instructions before it are ready to commit AND the ready bit for its
  // destination is set.
  //
  // TODO: AND the previous instruction is not a mispredicted branch...
  //
  // couldCommit(i) corresponds to front(i)
  val couldCommit = Vec.fill(4) { Bool() }
  for(i <- 0 until 4) {
    couldCommit(i) := (if(i > 0) { couldCommit(i-1) } else { Bool(true) }) && front(i).rdVal.valid
  }

  // If the head of the ROB is a store, tell the L/SQ to actually write to
  // memory now that the store has committed.
  for(i <- 0 until 4) {
    io.stCommit(i) := rob(head + i).isSt && couldCommit(i)
  }

  // If the head of the ROB is a mispredicted branch...
  for(i <- 0 until 4) {
    // TODO: this doesn't exactly work. We need to choose the first
    // mispredicted branch, not all of them...
    when (couldCommit(i) && front(i).isMispredicted) {
      io.mispredPC.valid := Bool(true)
      io.mispredPC.bits := front(i).pc
      io.mispredTarget := front(i).rdVal.bits
    } .otherwise {
      io.mispredPC.valid := Bool(false)
      io.mispredPC.bits := UInt(0)
      io.mispredTarget := UInt(0)
    }
  }

  // Write to the register file
  for(i <- 0 until 4) {
    // TODO: want to only write the last value out of all ready instructions.
    // i.e. if multiple committing instructions want to write the same reg,
    // only the last write should win...
    rf.wPorts(i).valid := couldCommit(i) && front(i).hasRd
    rf.wPorts(i).bits := front(i).rdVal.bits
  }
}

class ROBTests(c: ROB) extends Tester(c) {
  println("TODO")
}

class ROBGenerator extends TestGenerator {
  def genMod(): Module = Module(new ROB())
  def genTest[T <: Module](c: T): Tester[T] =
    (new ROBTests(c.asInstanceOf[ROB])).asInstanceOf[Tester[T]]
}
