package riscy

import Chisel._

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Vec.fill(4) {Valid(new AllocROB).asInput}
		val totalEntries = UInt(INPUT, 2)
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 4)
		// Currently just assume that below signals contain insts
		// issued in last 2 cycles as form of ROB entry
		// At top level this structure has to be generated - TODO
		val issuedPrev2 = Vec.fill(8) {Valid(UInt(INPUT, 6))}
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Reg(outType = new AllocROB)}

	val counter = new MultiCounter(16)
	
	// Assigning entries to current issue queue entries
	// New entries are assigned only if valid bit is set
	for (i <- 0 until 4) {
		when (io.newEntry(i).valid) {
			iqueue(counter.value + UInt(i)) := io.newEntry(i).bits
		}
	}
	
	// Logic for speculative wakeup
	// There is no speculative wake up, here it is assumed that
	// no stall happens within execution stage & 1 cycle execution
	// directly setting valid bit of rs1 and rs2 valid to true
	// No back to back execultion is supported for LSQ dependent
	// instructions which can be supported by following method
	// update issuedPrev array to 10 bits as ROB_WB structure
	for (i <- 0 until 7) {
		when (io.issuedPrev2(i).valid === Bool(true)) {
			for (j <- 0 until 15) {
				when (iqueue(j).rs1Val.valid === Bool(false) && 
				      iqueue(j).rs1Rename === io.issuedPrev2(i).bits) {
					iqueue(j).rs1Val.valid === Bool(true) 
				}
				when (iqueue(j).rs2Val.valid === Bool(false) && 
				      iqueue(j).rs2Rename === io.issuedPrev2(i).bits) {
					iqueue(j).rs2Val.valid === Bool(true) 
				}
			}
		}
	}


	// Updating counter value based on issued instruction
	// & number of new entries
	// Top level has to make sure all entries are assigned
	// in chronological order
	when (io.newEntry(0).valid && !io.issuedEntry.valid) {
  		counter.inc(io.totalEntries)
 	} .elsewhen (io.newEntry(0).valid && !io.issuedEntry.valid) {
		counter.inc(io.totalEntries - UInt(1))
  	} .elsewhen (!io.newEntry(0).valid && io.issuedEntry.valid) {
   	 	counter.dec(1)
	}

	io.currentLen := counter.value

	val allReady = Vec.tabulate(16) {
		i => iqueue(i).rs1Val.valid && 
		     iqueue(i).rs2Val.valid
	}
	
	// Issuing the oldest ready instruction
	// If none of the instructions are ready queue will give 
	// oldest instrution with valid bit not set
	val issuedNum = PriorityEncoder(allReady)
	val issuedNumOH = UIntToOH(issuedNum)
	io.issuedEntry := MuxLookup(issuedNum, iqueue(0), 
	Array.tabulate(16) {i => UInt (i) -> iqueue(i)})

//	val lastIssued = Bool(io.issuedEntry.valid)
	
	// Logic to shift instructions up in the order
	// if ith instruction is issued, we have to shift 
	// from i+1th instruction to 15th instruction one place up
	// shift will happen only if one instruction is issued with valid
//	val sum = Vec.fill(16) { UInt(width = 4)}  
	for (i <- 0 to 14) {
		when ((issuedNumOH(i) === UInt (1))&&io.issuedEntry.valid) {
			for (j <- i to 14) {
				iqueue(i) := iqueue(i+1)
			}
		} 
	}
}

class IssueQueueTests(c: IssueQueue) extends Tester(c) {
  println("TODO")
}

class IssueQueueGenerator extends TestGenerator {
  def genMod(): Module = Module(new IssueQueue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueQueueTests(c.asInstanceOf[IssueQueue])).asInstanceOf[Tester[T]]
}
