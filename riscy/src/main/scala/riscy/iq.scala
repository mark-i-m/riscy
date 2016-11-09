package riscy

import Chisel._

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Vec.fill(4) {Valid(new AllocROB).asInput}
		val totalEntries = UInt(INPUT, 2)
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 4)
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Reg(outType = Valid(new AllocROB))}

        
	val counter = new CounterUpDown(16)
	
	// Assigning entries to current issue queue entries
	for (i <- 0 until 4) {
		when (io.newEntry(i).valid) {
			iqueue(counter.value + UInt(i)).valid := Bool(true)
			iqueue(counter.value + UInt(i)).bits := io.newEntry(i).bits
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
			// TODO: speculative wakeup
		i => iqueue(i).valid &&
		     iqueue(i).bits.rs1Val.valid && 
		     iqueue(i).bits.rs2Val.valid
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
