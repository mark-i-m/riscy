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
    val robFree = UInt(OUTPUT, 6) // How many free entries
    val robFirst = UInt(OUTPUT, 6) // Index of the first free entry

    val allocRemap = Vec.fill(4) { Valid(new AllocRemap()).flip }
    val allocROB = Vec.fill(4) { Valid(new AllocROB()).flip }

    // Get signals from the FOO for WB
    val fooALU0 = new WBValue
    val fooALU1 = new WBValue
    val fooALU2 = new WBValue
    val fooALU3 = new WBValue
    val fooLSQ  = new WBValue

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

  // The Architectural register file
  val rf = Module(new RegFile(32, 8, 4, i => UInt(width = 64)))

  // The ROB storage structure
  val robW = Vec.fill(64) { Valid(new ROBEntry) } // Write enable and write value for ROB
  val rob = Vec.tabulate(64) { i => RegEnable(robW(i).bits, robW(i).valid) }

  val head = new MultiCounter(64) // most senior
  val headInc = UInt();
  val tail = new MultiCounter(64) // most recent (last valid instr if any)
  val tailInc = UInt();

  // # free entries
  var freeInc = UInt();
  val free = Reg(init = UInt(0), next = freeInc)

  when(io.mispredPC.valid) {
    freeInc := UInt(64)
    head.reset
    tail.reset
  } .otherwise {
    freeInc := free + headInc - tailInc
    head.inc(headInc)
    tail.inc(tailInc)
  }

  // TODO: add stalling logic

  // Allocation logic
  io.robFirst := tail.value + UInt(1)
  io.robFree  := free

  for(i <- 0 until 8) {
    remap.io.rPorts(i) := io.remapPorts(i)
    io.remapMapping(i) := remap.io.rValues(i)

    rf.io.rPorts(i) := io.rfPorts(i)
    io.rfValues(i) := rf.io.rValues(i)

    io.robDest(i) := rob(io.robPorts(i)).rdVal
  }

  // Latch new remap table mappings
  for(i <- 0 until 4) {
    remap.io.wPorts(i).valid := io.allocRemap(i).valid
    remap.io.wPorts(i).bits  := io.allocRemap(i).bits.reg
    remap.io.wValues(i)      := io.allocRemap(i).bits.idxROB
  }

  // Latch new ROB entries
  val back = Vec.tabulate(4) { i => robW(tail.value + UInt(i+1)) }
  back <> Array.tabulate(4) { io.allocROB(_) }
  // TODO: get ready operand values, too

  // Update tail pointer
  tailInc := PopCount(Array.tabulate(4) { back(_).valid })

  // Commit logic
  // - 4-wide in order commit
  // - Only commit if the head of the queue has a ready destination bit
  // - Stores should actually make their memory requests at this time
  // - Instructions with an rd should actually write the Arch reg file
  // - Branches that were mispredicted should trigger cleanup at this point
  //   (TODO: possibly can be done earlier?)
  // - Move the head pointer
  
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
    Array.tabulate(4) { i => UInt(i) -> front(i).rdVal.bits})

  // Write to the register file
  // 
  // - For each destination register among the four committing instructions,
  //   find out which is the last to rename that register.
  // - If this instruction is the last to rename, then write to RF
  val lastToRename = Vec.tabulate(4) { i =>
    PriorityEncoder((Array.tabulate(4) { j =>
      front(i).rd === front(j).rd && front(j).hasRd
    }).reverse)
  }

  for(i <- 0 until 4) {
    // Want to only write the last value out of all ready instructions.
    // i.e. if multiple committing instructions want to write the same reg,
    // only the last write should win...
    rf.io.wPorts(i).valid := couldCommit(i) && 
                             front(i).hasRd && 
                             lastToRename(i) === UInt(i)
    rf.io.wPorts(i).bits := front(i).rdVal.bits
  }

  // Compute the new head
  headInc := PopCount(couldCommit)
}

class ROBTests(c: ROB) extends Tester(c) {
  println("TODO")
}

class ROBGenerator extends TestGenerator {
  def genMod(): Module = Module(new ROB())
  def genTest[T <: Module](c: T): Tester[T] =
    (new ROBTests(c.asInstanceOf[ROB])).asInstanceOf[Tester[T]]
}

//  // The ROB storage structure
//  class CirculrBufferIterator[T](buffer:Array[T], start:Int) extends Iterator[T]{
//    var idx=0
//    override def hasNext = idx<buffer.size
//    override def next()={
//      val i=idx
//      idx=idx+1
//      buffer(i)
//    } 
//  }
//  
//  class CircularBuffer[T](size:Int)(implicit m:Manifest[T]) extends Seq[T]{
//    val buffer=new Array[T](size);
//    var bIdx=0;
//  
//    override def apply (idx: Int): T = buffer((bIdx+idx) % size)
//
//    override def length = size
//
//    override def iterator= new CirculrBufferIterator[T](buffer, bIdx)
//
//    def add(e:T)= {
//      buffer(bIdx)=e
//      bIdx=(bIdx +1) % size
//    }
//  }
//  
//  val rob = new CircularBuffer(64) { new ROBEntry() }
//  val head = Reg(next = UInt(width = 6))


