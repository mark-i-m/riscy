package riscy

import Chisel._

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Vec.fill(4) {Valid(new ROBEntry).asInput}
		// Currently just assume that below signals contain insts
		// issued in last 2 cycles as form of ROB entry
		val issuedPrev2 = Vec.fill(8) {Valid(UInt(INPUT, 6)).asInput}
		// Values from rob_wb with valid signal
		val robWb = new RobWbStore(6).flip
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 5)
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Reg(outType = (Valid(new ROBEntry)))}
	
	//Counter to maintain length of IQ
	val counter = new MultiCounter(17)

	//CAM to update RS1, RS2 values based on ROB_wb
	val wbCamRs1 = Module (new CAM(6, 16, 6))
	val wbCamRs2 = Module (new CAM(6, 16, 6))

	for (i <- 0 until 16) {
		wbCamRs1.io.input_bits(i) := iqueue(i).bits.rs1Rename
		wbCamRs2.io.input_bits(i) := iqueue(i).bits.rs2Rename
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
			iqueue(counter.value + isAssigned(i)) := io.newEntry(i)
			isAssigned(i+1) := isAssigned(i) + UInt(1)
		} .otherwise {
			isAssigned(i+1) := isAssigned(i) + UInt(0)
		}
	}
 
	//val totalEntries = isAssigned(0) + isAssigned(1) + isAssigned(2) + isAssigned(3)
  val isNewEntry = ( io.newEntry(0).valid || 
									 io.newEntry(1).valid || 
							 	   io.newEntry(2).valid || 
						  	   io.newEntry(3).valid )
	
	// Logic for speculative wakeup
	// There is no speculative wake up, here it is assumed that
	// no stall happens within execution stage & 1 cycle execution
	// directly setting valid bit of rs1 and rs2 valid to true
	// No back to back execultion is supported for LSQ dependent
	// instructions which can be supported by following method
	// update issuedPrev array to 10 bits as ROB_WB structure
	
	val isWokenUpRs1 = Vec.fill(16) {Vec.fill(9) {Bool()}}
	for (j <- 0 to 15) {
		isWokenUpRs1(j)(0) := Bool(false)
		for (i <- 0 to 7) {
			isWokenUpRs1(j)(i+1) := isWokenUpRs1(j)(i) || 
													(io.issuedPrev2(i).valid === Bool(true) && 
					    						 iqueue(j).bits.rs1Rename === io.issuedPrev2(i).bits &&
													 iqueue(j).valid)
		}
	}

	val isWokenUpRs2 = Vec.fill(16) {Vec.fill(9) {Bool()}}
	for (j <- 0 to 15) {
		isWokenUpRs2(j)(0) := Bool(false)
		for (i <- 0 to 7) {
			isWokenUpRs2(j)(i+1) := isWokenUpRs2(j)(i) || 
													(io.issuedPrev2(i).valid === Bool(true) && 
					    						 iqueue(j).bits.rs2Rename === io.issuedPrev2(i).bits &&
													 iqueue(j).valid)
		}
	}

	val wakeUpRs1 = Vec.fill(16) {Bool()}
	for (j <- 0 to 15) {
		when (isWokenUpRs1(j)(8) === Bool(true)) {
			wakeUpRs1(j) := Bool(true)
		} .otherwise {
			wakeUpRs1(j) := iqueue(j).bits.rs1Val.valid
		}
		//printf("Rs1WakeUp ROB%d val: %x\n", UInt(j), wakeUpRs1(j))
	}

	val wakeUpRs2 = Vec.fill(16) {Bool()}
	for (j <- 0 to 15) {
		when (isWokenUpRs2(j)(8) === Bool(true)) {
			wakeUpRs2(j) := Bool(true)
		} .otherwise {
			wakeUpRs2(j) := iqueue(j).bits.rs2Val.valid
		}
		//printf("Rs2WakeUp ROB%d val: %x\n", UInt(j), wakeUpRs2(j))
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
	val issuedPipelineBits = Reg(next = MuxLookup(issuedNum, iqueue(0).bits, 
	Array.tabulate(16) {i => UInt (i) -> iqueue(i).bits}))

	//printf("issuedNum val: %x\n issuedNumOH %x\n", issuedNum, issuedNumOH)
	
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
						when (wbCamRs1.io.hit(k)(j+1) && io.robWb.valid_s1(k) && iqueue(j).valid) {
							iqueue(j).bits.rs1Val.bits := io.robWb.data_s1(k)
							iqueue(j).bits.rs1Val.bits := Bool(true)
						}
						when (wbCamRs2.io.hit(k)(j+1) && io.robWb.valid_s1(k) && iqueue(j).valid) {
							iqueue(j).bits.rs2Val.bits := io.robWb.data_s1(k)
							iqueue(j).bits.rs2Val.valid := Bool(true)
						} 	
					}
					iqueue(15).bits.rs1Val.valid := Bool(false)
					iqueue(15).bits.rs2Val.valid := Bool(false)
				}
				for (l <- 0 to i-1) {
					for (k <- 0 until 6) {
						when (wbCamRs1.io.hit(k)(l) && io.robWb.valid_s1(k) && iqueue(l).valid) {
							iqueue(l).bits.rs1Val.bits := io.robWb.data_s1(k)
							iqueue(l).bits.rs1Val.bits := Bool(true)
						}
						when (wbCamRs2.io.hit(k)(l) && io.robWb.valid_s1(k) && iqueue(l).valid) {
							iqueue(l).bits.rs2Val.bits := io.robWb.data_s1(k)
							iqueue(l).bits.rs2Val.valid := Bool(true)
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
				when (wbCamRs1.io.hit(k)(l) && io.robWb.valid_s1(k) && iqueue(l).valid) {
					iqueue(l).bits.rs1Val.bits := io.robWb.data_s1(k)
					iqueue(l).bits.rs1Val.bits := Bool(true)
				}
				when (wbCamRs2.io.hit(k)(l) && io.robWb.valid_s1(k) && iqueue(l).valid) {
					iqueue(l).bits.rs2Val.bits := io.robWb.data_s1(k)
					iqueue(l).bits.rs2Val.valid := Bool(true)
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
	poke(c.io.issuedPrev2(0).valid,1)
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

	println("// Test7e")
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

	println("// Test8 - Adding test to completely fill IQ")
	println("// write back a value and issue one instruction at a time")
	
	poke(c.io.robWb.operand_s1(0), 4)
	poke(c.io.robWb.valid_s1(0), 0)
	
	for (j <- 0 to 3) {
		for (i <- 0 to 3) {
			poke(c.io.newEntry(i).valid, 1)
			poke(c.io.newEntry(i).bits.tag, (4*j)+i)
			poke(c.io.newEntry(i).bits.rs1Val.valid, 1)
			poke(c.io.newEntry(i).bits.rs2Val.valid, 0)
			poke(c.io.newEntry(i).bits.rs2Rename, 5)
		}
		
		for (i <- 0 to 7) {
			poke(c.io.issuedPrev2(i).valid,0)
	  }
		step(1)
		expect(c.io.currentLen, 4*(j+1))
		expect(c.io.issuedEntry.valid, 0)
		expect(c.io.issuedEntry.bits.tag, 0)
	}
	
	println("// Test8a")
	
	for (i <- 0 to 7) {
			poke(c.io.issuedPrev2(i).valid,0)
	}	
	
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.operand_s1(0), 5)
	poke(c.io.robWb.valid_s1(0), 1)

	step(1)
	expect(c.io.currentLen, 16)
	expect(c.io.issuedEntry.valid, 0)
  
	println("// Test8b")
	
	poke(c.io.robWb.operand_s1(0), 4)
	poke(c.io.robWb.valid_s1(0), 0)
	
	for (i <- 0 to 15) {
		for (i <- 0 to 7) {
			poke(c.io.issuedPrev2(i).valid,0)
		}
		
		poke(c.io.newEntry(0).valid, 0)
		poke(c.io.newEntry(1).valid, 0)
		poke(c.io.newEntry(2).valid, 0)
		poke(c.io.newEntry(3).valid, 0)

		step(1)
		expect(c.io.currentLen, (15-i))
		expect(c.io.issuedEntry.valid, 1)
		expect(c.io.issuedEntry.bits.tag, i)
	}
	
	println("// Test8c")
	
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

	println("// Test9 - 1st issue 2nd instruction, followed by 1st")
	
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 1)
	poke(c.io.newEntry(0).bits.tag, 0)
	poke(c.io.newEntry(0).bits.rs1Val.valid, 0)
	poke(c.io.newEntry(0).bits.rs2Val.valid, 1)
	poke(c.io.newEntry(0).bits.rs1Rename, 1)

	poke(c.io.newEntry(1).valid, 1)
	poke(c.io.newEntry(1).bits.tag, 1)
	poke(c.io.newEntry(1).bits.rs1Val.valid, 1)
	poke(c.io.newEntry(1).bits.rs2Val.valid, 1)

	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	
	expect(c.io.currentLen, 2)
	expect(c.io.issuedEntry.valid, 0)

	println("// Test9b")

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
	expect(c.io.issuedEntry.bits.tag, 1)

	println("// Test9c")
	
	poke(c.io.issuedPrev2(7).valid,1)
	poke(c.io.issuedPrev2(7).bits,1)
	for (i <- 0 to 6) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 0)

	println("// Test9c")
	
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

	println("// Test10")
	println("//	 - check if non filled issue queue are generating a valid signal")
	poke(c.io.robWb.operand_s1(0), 0)
	poke(c.io.robWb.valid_s1(0), 1)

	step(1)
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 0)

}

class IssueQueueGenerator extends TestGenerator {
  def genMod(): Module = Module(new IssueQueue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueQueueTests(c.asInstanceOf[IssueQueue])).asInstanceOf[Tester[T]]
}
