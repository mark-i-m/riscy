package riscy

import Chisel._

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Vec.fill(4) {Valid(new ROBEntry).asInput}
		val iqueue0Tag = UInt(OUTPUT, 6)
		// Currently just assume that below signals contain insts
		// issued in last 2 cycles as form of ROB entry
		// At top level this structure has to be generated - TODO
		val issuedPrev2 = Vec.fill(8) {Valid(UInt(INPUT, 6)).asInput}
		// Values from rob_wb with valid signal
		val robWb = new RobWbStore(6).flip
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 5)
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Reg(outType = new ROBEntry)}
	
	//Counter to maintain length of IQ
	val counter = new MultiCounter(17)

	//CAM to update RS1, RS2 values based on ROB_wb
	val wbCamRs1 = Module (new CAM(6, 16, 6))
	val wbCamRs2 = Module (new CAM(6, 16, 6))

	for (i <- 0 until 16) {
		wbCamRs1.io.input_bits(i) := iqueue(i).rs1Rename
		wbCamRs2.io.input_bits(i) := iqueue(i).rs2Rename
	}
	for (i <- 0 until 6) {
		wbCamRs1.io.compare_bits(i) := io.robWb.operand_s1(i)
		wbCamRs2.io.compare_bits(i) := io.robWb.operand_s1(i)
	}
  
	val isAssigned = Vec.fill(5) {UInt(width = 3)}
	// Assigning entries to current issue queue entries
	// New entries are assigned only if valid bit is set
	isAssigned(0) := UInt(0)
	for (i <- 0 until 4) {
		when (io.newEntry(i).valid) {
			iqueue(counter.value + isAssigned(i)) := io.newEntry(i).bits
			isAssigned(i+1) := isAssigned(i) + UInt(1)
		} .otherwise {
			isAssigned(i+1) := isAssigned(i) + UInt(0)
		}
	}

	io.iqueue0Tag := iqueue(0).tag
 
	//val totalEntries = isAssigned(0) + isAssigned(1) + isAssigned(2) + isAssigned(3)
  val isNewEntry = ( io.newEntry(0).valid || 
									 io.newEntry(1).valid || 
							 	   io.newEntry(2).valid || 
						  	   io.newEntry(3).valid )
	
	val wakeUpRs1 = Vec.tabulate(16) {i => iqueue(i).rs1Val.valid}
	val wakeUpRs2 = Vec.tabulate(16) {i => iqueue(i).rs2Val.valid}

	// Logic for speculative wakeup
	// There is no speculative wake up, here it is assumed that
	// no stall happens within execution stage & 1 cycle execution
	// directly setting valid bit of rs1 and rs2 valid to true
	// No back to back execultion is supported for LSQ dependent
	// instructions which can be supported by following method
	// update issuedPrev array to 10 bits as ROB_WB structure
	for (j <- 0 to 15) {
		for (i <- 0 to 7) {
			when (io.issuedPrev2(i).valid === Bool(true)) {
				when (iqueue(j).rs1Val.valid === Bool(false) && 
				      iqueue(j).rs1Rename === io.issuedPrev2(i).bits) {
					wakeUpRs1(j) := Bool(true) 
				} 
				when (iqueue(j).rs2Val.valid === Bool(false) && 
				      iqueue(j).rs2Rename === io.issuedPrev2(i).bits) {
					wakeUpRs2(j) := Bool(true) 
				} 
			}
		} 
	}
	
	
	// this is to check which all insts are ready
	val allReady = Vec.tabulate(16) {
		i =>  wakeUpRs1(i) && wakeUpRs2(i)
	}
	
	// to check if there exists an issuable instruction 
	val isIssued = Vec.fill(17) {Bool()}
	isIssued(0) := Bool(false)
	for (i <- 0 to 15) {
		isIssued(i+1) := isIssued(i) || allReady(i)
	}
	val issuedPipelineValid = isIssued(16)
	io.issuedEntry.valid := Reg(next = issuedPipelineValid)
	
	// Updating counter value based on issued instruction
	// & number of new entries
	// Top level has to make sure all entries are assigned
	// in chronological order
	when (isNewEntry && !issuedPipelineValid) {
  	counter.inc(isAssigned(4))
 	} .elsewhen (isNewEntry && issuedPipelineValid) {
		counter.inc(isAssigned(4) - UInt(1))
  } .elsewhen (!isNewEntry && issuedPipelineValid) {
   	counter.dec(1)
	}
	
	io.currentLen := counter.value

	// Issuing the oldest ready instruction
	// If none of the instructions are ready queue will give 
	// oldest instrution with valid bit not set
	val issuedNum = PriorityEncoder(allReady)
	val issuedNumOH = UIntToOH(issuedNum)
	val issuedPipelineBits = Reg(next = MuxLookup(issuedNum, iqueue(0), 
	Array.tabulate(16) {i => UInt (i) -> iqueue(i)}))
	
	io.issuedEntry.bits := issuedPipelineBits

	// First case is when instruction is issued from issue queue
	// Logic to shift instructions up in the order
	// if ith instruction is issued, we have to shift 
	// from i+1th instruction to 15th instruction one place up
	// shift will happen only if one instruction is issued with valid
	when (issuedPipelineValid === Bool(true)) {
		for (i <- 0 to 14) {
			//Code when there are atleast 
			when (issuedNumOH(i) === UInt (1)) {
				for (j <- i to 14) {
					iqueue(j) := iqueue(j+1)
					// Logic to write rob_wb data into issue queue
					// till the entry of issued directly compare with wb value
					// as there will be no shift
					// from issued to last entry compare with next one 
					// as they are shifted
					for (k <- 0 until 6) {
						when (wbCamRs1.io.hit(k)(j+1) && io.robWb.valid_s1(k)) {
							iqueue(j).rs1Val.bits := io.robWb.data_s1(k)
							iqueue(j).rs1Val.bits := Bool(true)
						}
						when (wbCamRs2.io.hit(k)(j+1) && io.robWb.valid_s1(k)) {
							iqueue(j).rs2Val.bits := io.robWb.data_s1(k)
							iqueue(j).rs2Val.valid := Bool(true)
						} 	
					}
				}
				for (l <- 0 to i-1) {
					for (k <- 0 until 6) {
						when (wbCamRs1.io.hit(k)(l) && io.robWb.valid_s1(k)) {
							iqueue(l).rs1Val.bits := io.robWb.data_s1(k)
							iqueue(l).rs1Val.bits := Bool(true)
						}
						when (wbCamRs2.io.hit(k)(l) && io.robWb.valid_s1(k)) {
							iqueue(l).rs2Val.bits := io.robWb.data_s1(k)
							iqueue(l).rs2Val.valid := Bool(true)
						} 	
					}
				}
			} 
		}
	} .otherwise {
		// if there is no instruction issued just take wb data into 
		// IQ entries
		for (l <- 0 to 15) {
			for (k <- 0 until 6) {
				when (wbCamRs1.io.hit(k)(l) && io.robWb.valid_s1(k)) {
					iqueue(l).rs1Val.bits := io.robWb.data_s1(k)
					iqueue(l).rs1Val.bits := Bool(true)
				}
				when (wbCamRs2.io.hit(k)(l) && io.robWb.valid_s1(k)) {
					iqueue(l).rs2Val.bits := io.robWb.data_s1(k)
					iqueue(l).rs2Val.valid := Bool(true)
				} 	
			}
		}
	}
}

class IssueQueueTests(c: IssueQueue) extends Tester(c) {
	println("// Test0 - in default state check current value of counter")
	expect(c.io.currentLen, 0x0)
	expect(c.io.issuedEntry.valid, 0)

	println("// Test1 - assigning 4 enries to issue queue")
	for (i <- 0 to 3) {
		poke(c.io.newEntry(i).valid, 1)
		poke(c.io.newEntry(i).bits.tag, i)
		poke(c.io.newEntry(i).bits.rs1Val.valid, 1)
		poke(c.io.newEntry(i).bits.rs2Val.valid, 1)
	}
	//poke(c.io.newEntry(1).valid, 1)
	//poke(c.io.newEntry(2).valid, 1)
	//poke(c.io.newEntry(3).valid, 1)
	
	step(1)
	
	expect(c.io.currentLen, 4)
	expect(c.io.issuedEntry.valid, 0)

	println("// Test2 - no new entries, issue queue should issue one entry")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 3)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 0)

	println("// Test3 - no new entries, issue queue should issue one entry")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 2)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 1)

	println("// Test4 - no new entries, issue queue should issue one entry")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 1)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 2)

	println("// Test5 - no new entries, issue queue should issue one entry")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 3)

	println("// Test6 - no new entries, issue queue should issue one entry")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 0)

	println("// Test7 - fillup with invalid instruction & issue 1 by 1") 
	
	for (i <- 0 to 3) {
		poke(c.io.newEntry(i).valid, 1)
		poke(c.io.newEntry(i).bits.tag, i)
		poke(c.io.newEntry(i).bits.rs1Val.valid, 1)
		poke(c.io.newEntry(i).bits.rs2Val.valid, 0)
		poke(c.io.newEntry(i).bits.rs2Rename, 5)
	}
	
	step(1)
	expect(c.io.currentLen, 4)
	expect(c.io.issuedEntry.valid, 0)
	
	println("// Test7a")
	poke(c.io.issuedPrev2(0).bits,5)
	poke(c.io.issuedPrev2(0).valid,0)
	for (i <- 1 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
  poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.operand_s1(0), 5)
	poke(c.io.robWb.valid_s1(0), 1)

	step(1)
	expect(c.io.currentLen, 3)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 0)

	println("// Test7b")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
  poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 2)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 1)

	println("// Test7c")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
  poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 1)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 2)

	println("// Test7d")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
  poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 3)

}

class IssueQueueGenerator extends TestGenerator {
  def genMod(): Module = Module(new IssueQueue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueQueueTests(c.asInstanceOf[IssueQueue])).asInstanceOf[Tester[T]]
}
