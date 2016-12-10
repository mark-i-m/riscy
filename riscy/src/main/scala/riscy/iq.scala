package riscy

import Chisel._

class SpeculativeIssue extends Bundle {
	val rs1WbLocation = UInt(OUTPUT, 2)
	val rs1CycleNum 	= UInt(OUTPUT, 1)
	val rs1IsSpec 		= Bool(OUTPUT)
	val rs2WbLocation = UInt(OUTPUT, 2)
	val rs2CycleNum 	= UInt(OUTPUT, 1)
	val rs2IsSpec	 		= Bool(OUTPUT)
}

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Vec.fill(4) {Valid(new ROBEntry).asInput}
		// Currently just assume that below signals contain insts
		// issued in last 2 cycles as form of ROB entry
		val issuedPrev2 = Vec.fill(8) {Valid((new IssuedPrev2Inst()).flip).asInput}
		// Values from rob_wb with valid signal
		val robWb = new RobWbStore(6).flip
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 5)
		val specIssue = Valid(new SpeculativeIssue).asOutput
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Reg(outType = (Valid(new ROBEntry)))}
	
	//Counter to maintain length of IQ
	val counter = new MultiCounter(17)

	//CAM to update RS1, RS2 values based on ROB_wb
	val wbCamRs1 = Module (new CAM(6, 20, 6))
	val wbCamRs2 = Module (new CAM(6, 20, 6))

	for (i <- 0 until 16) {
		wbCamRs1.io.input_bits(i) := iqueue(i).bits.rs1Rename
		wbCamRs2.io.input_bits(i) := iqueue(i).bits.rs2Rename
	}
	for (i <- 16 until 20) {
		wbCamRs1.io.input_bits(i) := io.newEntry(i-16).bits.rs1Rename
    wbCamRs2.io.input_bits(i) := io.newEntry(i-16).bits.rs2Rename
	}
	for (i <- 0 until 6) {
		wbCamRs1.io.compare_bits(i) := io.robWb.entry_s1(i).operand
		wbCamRs2.io.compare_bits(i) := io.robWb.entry_s1(i).operand
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
	// added support for not doing speculative wakeup when last issued was a load
	
	val isWokenUpRs1 = Vec.fill(16) {Vec.fill(9) {Bool()}}
	for (j <- 0 to 15) {
		isWokenUpRs1(j)(0) := Bool(false)
		for (i <- 0 to 7) {
			isWokenUpRs1(j)(i+1) := isWokenUpRs1(j)(i) || 
													(io.issuedPrev2(i).valid === Bool(true) &&
													 iqueue(j).bits.rs1Val.valid === Bool(false) &&
													 io.issuedPrev2(i).bits.isLd === Bool(false) &&
					    						 iqueue(j).bits.rs1Rename === io.issuedPrev2(i).bits.tag &&
													 iqueue(j).valid)
		}
	}

	val isWokenUpRs2 = Vec.fill(16) {Vec.fill(9) {Bool()}}
	for (j <- 0 to 15) {
		isWokenUpRs2(j)(0) := Bool(false)
		for (i <- 0 to 7) {
			isWokenUpRs2(j)(i+1) := isWokenUpRs2(j)(i) || 
													(io.issuedPrev2(i).valid === Bool(true) && 
													 iqueue(j).bits.rs2Val.valid === Bool(false) &&
													 io.issuedPrev2(i).bits.isLd === Bool(false) &&
					    						 iqueue(j).bits.rs2Rename === io.issuedPrev2(i).bits.tag &&
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
	

	// Issuing the oldest ready instruction
	// If none of the instructions are ready queue will give 
	// oldest instrution with valid bit not set
	val issuedNum = PriorityEncoder(allReady)
	val issuedNumOH = UIntToOH(issuedNum)
	val issuedPipelineBits = Reg(next = MuxLookup(issuedNum, iqueue(0).bits, 
	Array.tabulate(16) {i => UInt (i) -> iqueue(i).bits}))

	// for 0th instruction it is possible that rs1 and rs2 valid have stuck at one fault
	// so check if issued inst is valid or not
	val issuedPipelineValid = isIssued(16) 
	io.issuedEntry.valid := Reg(next = issuedPipelineValid)
 
	// Logic to provide speculative location to execute
	val issuedPrev2Pipeline = Vec.tabulate(8) { i => Reg(next = io.issuedPrev2(i)) }
	val specCamRs1 = Module (new CAM(1, 8, 6))
	val specCamRs2 = Module (new CAM(1, 8, 6))
	
	specCamRs1.io.compare_bits(0) := issuedPipelineBits.rs1Rename
	specCamRs2.io.compare_bits(0) := issuedPipelineBits.rs2Rename
	
	for (i <- 0 until 8) {
		specCamRs1.io.input_bits(i) := issuedPrev2Pipeline(i).bits.tag
		specCamRs2.io.input_bits(i) := issuedPrev2Pipeline(i).bits.tag
	}

	val specInfo = Valid(new SpeculativeIssue()) 

	specInfo.bits.rs1WbLocation		:= UInt(0)
  specInfo.bits.rs1CycleNum 		:= UInt(0)
	specInfo.bits.rs2WbLocation		:= UInt(0)
  specInfo.bits.rs2CycleNum 		:= UInt(0)

	// Generating speculative bit value
	when (issuedPipelineBits.rs1Val.valid === Bool(false)) {
		specInfo.bits.rs1IsSpec 			:= Bool(true) 
	} .otherwise {
		specInfo.bits.rs1IsSpec			 	:= Bool(false)
	}
	
	// Logic to rovide info to EXE logic about WB location
	for (i <- 0 until 8) {
		when (specCamRs1.io.hit(0)(i) && issuedPrev2Pipeline(i).valid) {
			if (i < 4) {
				specInfo.bits.rs1WbLocation		:= UInt(i) 
				specInfo.bits.rs1CycleNum 		:= UInt(0)
			} else {
				specInfo.bits.rs1WbLocation		:= UInt(i-4) 
				specInfo.bits.rs1CycleNum 		:= UInt(1)
			}
		} 
	}

	when (issuedPipelineBits.rs2Val.valid === Bool(false)) {
		specInfo.bits.rs2IsSpec 			:= Bool(true) 
	} .otherwise {
		specInfo.bits.rs2IsSpec 			:= Bool(false)
	}

	for (i <- 0 until 8) {
		when (specCamRs2.io.hit(0)(i) && issuedPrev2Pipeline(i).valid) {
			if (i < 4) {
				specInfo.bits.rs2WbLocation		:= UInt(i) 
				specInfo.bits.rs2CycleNum 		:= UInt(0)
			} else {
				specInfo.bits.rs2WbLocation		:= UInt(i-4) 
				specInfo.bits.rs2CycleNum 		:= UInt(1)
			}
		} 
	}

  // woken up inst is speculative if one of the rs1 or rs2 is not ready
	specInfo.valid := specInfo.bits.rs1IsSpec || specInfo.bits.rs2IsSpec
	
	//val specPipelined = Reg(next = specInfo)
	io.specIssue := specInfo

	//printf("issuedNum val: %x\n issuedNumOH %x\n", issuedNum, issuedNumOH)
	
	io.issuedEntry.bits := issuedPipelineBits
	
	val isAssigned = Vec.fill(5) {UInt(width = 3)}
	for (i <- 0 until 5) {
		isAssigned(i) := UInt(0)
	}
	
	// Assigning entries to current issue queue entries
	// New entries are assigned only if valid bit is set
	// also logic to update the new entry with ROB structure
	
	when (counter.value === UInt(0)) {
		for (i <- 0 until 4) {
			when (io.newEntry(i).valid) {
				iqueue(counter.value + isAssigned(i)) := io.newEntry(i)
				isAssigned(i+1) := isAssigned(i) + UInt(1)
				for (k <- 0 until 6) {
					when (wbCamRs1.io.hit(k)(16+i) && 
								io.robWb.entry_s1(k).valid && 
								!io.robWb.entry_s1(k).is_addr &&
								!io.newEntry(i).bits.rs1Val.valid) {
						iqueue(counter.value + isAssigned(i)).bits.rs1Val.bits := io.robWb.entry_s1(k).data
						iqueue(counter.value + isAssigned(i)).bits.rs1Val.valid := Bool(true)
					}
					when (wbCamRs2.io.hit(k)(16+i) && 
								io.robWb.entry_s1(k).valid && 
								!io.robWb.entry_s1(k).is_addr &&
								!io.newEntry(i).bits.rs2Val.valid) {
						iqueue(counter.value + isAssigned(i)).bits.rs2Val.bits := io.robWb.entry_s1(k).data
						iqueue(counter.value + isAssigned(i)).bits.rs2Val.valid := Bool(true)
					} 	
				}
			} .otherwise {
				isAssigned(i+1) := isAssigned(i) + UInt(0)
			}
		}
	} .otherwise {
		when (issuedPipelineValid === Bool(false)) {
			for (i <- 0 until 4) {
				when (io.newEntry(i).valid) {
					iqueue(counter.value + isAssigned(i)) := io.newEntry(i)
					isAssigned(i+1) := isAssigned(i) + UInt(1)
					for (k <- 0 until 6) {
						when (wbCamRs1.io.hit(k)(16+i) && 
								 	io.robWb.entry_s1(k).valid && 
									!io.robWb.entry_s1(k).is_addr &&
									!io.newEntry(i).bits.rs1Val.valid) {
							iqueue(counter.value + isAssigned(i)).bits.rs1Val.bits := io.robWb.entry_s1(k).data
							iqueue(counter.value + isAssigned(i)).bits.rs1Val.valid := Bool(true)
						}
						when (wbCamRs2.io.hit(k)(16+i) && 
									io.robWb.entry_s1(k).valid && 
									!io.robWb.entry_s1(k).is_addr &&
									!iqueue(counter.value + isAssigned(i)).bits.rs2Val.valid) {
							iqueue(counter.value + isAssigned(i)).bits.rs2Val.bits := io.robWb.entry_s1(k).data
							iqueue(counter.value + isAssigned(i)).bits.rs2Val.valid := Bool(true)
						} 	
					}
				} .otherwise {
					isAssigned(i+1) := isAssigned(i) + UInt(0)
				}
			}
		} .otherwise {
			for (i <- 0 until 4) {
				when (io.newEntry(i).valid) {
					iqueue(counter.value + isAssigned(i) - UInt(1)) := io.newEntry(i)
					isAssigned(i+1) := isAssigned(i) + UInt(1)
					for (k <- 0 until 6) {
						when (wbCamRs1.io.hit(k)(16+i) && 
									io.robWb.entry_s1(k).valid && 
									!io.robWb.entry_s1(k).is_addr &&
									!io.newEntry(i).bits.rs1Val.valid) {
							iqueue(counter.value + isAssigned(i) - UInt(1)).bits.rs1Val.bits := io.robWb.entry_s1(k).data
							iqueue(counter.value + isAssigned(i) - UInt(1)).bits.rs1Val.valid := Bool(true)
						}
						when (wbCamRs2.io.hit(k)(16+i) && 
									io.robWb.entry_s1(k).valid && 
									!io.robWb.entry_s1(k).is_addr &&
									!io.newEntry(i).bits.rs2Val.valid) {
							iqueue(counter.value + isAssigned(i) - UInt(1)).bits.rs2Val.bits := io.robWb.entry_s1(k).data
							iqueue(counter.value + isAssigned(i) - UInt(1)).bits.rs2Val.valid := Bool(true)
						} 	
					}
				} .otherwise {
					isAssigned(i+1) := isAssigned(i) + UInt(0)
				}
			}
		}
	}
	
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
	
	// First case is when instruction is issued from issue queue
	// Logic to shift instructions up in the order
	// if ith instruction is issued, we have to shift 
	// from i+1th instruction to 15th instruction one place up
	// shift will happen only if one instruction is issued with valid
	when (issuedPipelineValid === Bool(true)) {
		for (i <- 0 to 14) {
			//when (counter.value === UInt (1)) {
			//	iqueue(0) := iqueue(1)
			//}
			// Select if this instruction was issued
			when (issuedNumOH(i) === UInt (1)) {
				for (j <- i to 14) {
					when (isNewEntry) {	
						// Check till which value queue is filled
						// and transfer till all valid are transferred
						when (UInt(j) < (counter.value - UInt(1))) {
							iqueue(j) := iqueue(j+1)
		
							// Logic to write rob_wb data into issue queue
							// till the entry of issued directly compare with wb value
							// as there will be no shift
							// from issued to last entry compare with next one 
							// as they are shifted
							for (k <- 0 until 6) {
								when (wbCamRs1.io.hit(k)(j+1) && 
											io.robWb.entry_s1(k).valid && 
											iqueue(j+1).valid && 
											!io.robWb.entry_s1(k).is_addr &&
											!iqueue(j).bits.rs1Val.valid) {
									iqueue(j).bits.rs1Val.bits := io.robWb.entry_s1(k).data
									iqueue(j).bits.rs1Val.valid := Bool(true)
								}
								when (wbCamRs2.io.hit(k)(j+1) && 
											io.robWb.entry_s1(k).valid && 
											iqueue(j+1).valid && 
											!io.robWb.entry_s1(k).is_addr &&
											!iqueue(j).bits.rs2Val.valid) {
									iqueue(j).bits.rs2Val.bits := io.robWb.entry_s1(k).data
									iqueue(j).bits.rs2Val.valid := Bool(true)
								} 	
							}
							iqueue(15).bits.rs1Val.valid := Bool(false)
							iqueue(15).bits.rs2Val.valid := Bool(false)
						}
					} .otherwise {
						// Check till which value queue is filled
						// and transfer till all valid are transferred
						when (UInt(j) < counter.value) {
							iqueue(j) := iqueue(j+1)
		
							// Logic to write rob_wb data into issue queue
							// till the entry of issued directly compare with wb value
							// as there will be no shift
							// from issued to last entry compare with next one 
							// as they are shifted
							for (k <- 0 until 6) {
								when (wbCamRs1.io.hit(k)(j+1) && 
											io.robWb.entry_s1(k).valid && 
											iqueue(j+1).valid && 
											!io.robWb.entry_s1(k).is_addr &&
											!iqueue(j).bits.rs1Val.valid) {
									iqueue(j).bits.rs1Val.bits := io.robWb.entry_s1(k).data
									iqueue(j).bits.rs1Val.valid := Bool(true)
								}
								when (wbCamRs2.io.hit(k)(j+1) && 
											io.robWb.entry_s1(k).valid && 
											iqueue(j+1).valid && 
											!io.robWb.entry_s1(k).is_addr &&
											!iqueue(j).bits.rs2Val.valid) {
									iqueue(j).bits.rs2Val.bits := io.robWb.entry_s1(k).data
									iqueue(j).bits.rs2Val.valid := Bool(true)
								} 	
							}
							iqueue(15).bits.rs1Val.valid := Bool(false)
							iqueue(15).bits.rs2Val.valid := Bool(false)
						}
					}
				}
				for (l <- 0 to i-1) {
					for (k <- 0 until 6) {
						when (wbCamRs1.io.hit(k)(l) && 
									io.robWb.entry_s1(k).valid && 
									iqueue(l).valid && 
									!io.robWb.entry_s1(k).is_addr &&
									!iqueue(l).bits.rs1Val.valid) {
							iqueue(l).bits.rs1Val.bits := io.robWb.entry_s1(k).data
							iqueue(l).bits.rs1Val.valid := Bool(true)
						}
						when (wbCamRs2.io.hit(k)(l) && 
									io.robWb.entry_s1(k).valid && 
									iqueue(l).valid && 
									!io.robWb.entry_s1(k).is_addr &&
									!iqueue(l).bits.rs2Val.valid) {
							iqueue(l).bits.rs2Val.bits := io.robWb.entry_s1(k).data
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
				when (wbCamRs1.io.hit(k)(l) && 
							io.robWb.entry_s1(k).valid && 
							iqueue(l).valid && 
							!io.robWb.entry_s1(k).is_addr &&
							!iqueue(l).bits.rs1Val.valid) {
					iqueue(l).bits.rs1Val.bits := io.robWb.entry_s1(k).data
					iqueue(l).bits.rs1Val.valid := Bool(true)
				}
				when (wbCamRs2.io.hit(k)(l) && 
							io.robWb.entry_s1(k).valid && 
							iqueue(l).valid && 
							!io.robWb.entry_s1(k).is_addr &&
							!iqueue(l).bits.rs2Val.valid) {
					iqueue(l).bits.rs2Val.bits := io.robWb.entry_s1(k).data
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
	
	println("// Test1a - no new entries, issue queue should issue one entry")

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
	expect(c.io.specIssue.valid, 0)

	println("// Test1b - no new entries, issue queue should issue one entry")
	
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
	expect(c.io.specIssue.valid, 0)

	println("// Test1c - no new entries, issue queue should issue one entry")
	
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
	expect(c.io.specIssue.valid, 0)

	println("// Test1d - no new entries, issue queue should issue one entry")
	
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
	expect(c.io.specIssue.valid, 0)

	println("// Test1d - no new entries, issue queue should issue one entry")
	
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
	
	println("// Test2 - assigning 4 enries to issue queue")
	
	for (i <- 0 to 3) {
		poke(c.io.newEntry(i).valid, 1)
		poke(c.io.newEntry(i).bits.tag, i)
		poke(c.io.newEntry(i).bits.rs1Val.valid, 1)
		poke(c.io.newEntry(i).bits.rs2Val.valid, 1)
	}
	
	step(1)
	
	expect(c.io.currentLen, 4)
	expect(c.io.issuedEntry.valid, 0)
	
	println("// Test2a - no new entries, issue queue should issue one entry")
	
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	
	for (i <- 0 to 3) {
		poke(c.io.newEntry(i).valid, 1)
		poke(c.io.newEntry(i).bits.tag, 4+i)
		poke(c.io.newEntry(i).bits.rs1Val.valid, 1)
		poke(c.io.newEntry(i).bits.rs2Val.valid, 1)
	}
	
	step(1)
	
	expect(c.io.currentLen, 7)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 0)
	expect(c.io.specIssue.valid, 0)

	println("// Test2b - no new entries, issue queue should issue one entry")
	
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	
	expect(c.io.currentLen, 6)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 1)
	expect(c.io.specIssue.valid, 0)

	println("// Test2c - no new entries, issue queue should issue one entry")
	
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	
	expect(c.io.currentLen, 5)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 2)

	println("// Test2d - no new entries, issue queue should issue one entry")
	
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)

	step(1)
	
	expect(c.io.currentLen, 4)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 3)

	println("// Test2e - no new entries, issue queue should issue one entry")
	
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
	expect(c.io.issuedEntry.bits.tag, 4)

	println("// Test2f - no new entries, issue queue should issue one entry")
	
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
	expect(c.io.issuedEntry.bits.tag, 5)

	println("// Test2g - no new entries, issue queue should issue one entry")
	
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
	expect(c.io.issuedEntry.bits.tag, 6)

	println("// Test2h - no new entries, issue queue should issue one entry")
	
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
	expect(c.io.issuedEntry.bits.tag, 7)

	println("// Test2i - no new entries, issue queue should issue one entry")
	
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

	println("// Test3 - fillup with invalid instruction & issue 1 by 1") 
	
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
	
	println("// Test3a")
	
	poke(c.io.issuedPrev2(0).bits.tag,5)
	poke(c.io.issuedPrev2(0).valid,1)
	
	for (i <- 1 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
  poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.entry_s1(0).operand, 5)
	poke(c.io.robWb.entry_s1(0).valid, 1)
		
	step(1)
	
	expect(c.io.currentLen, 3)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 0)
	expect(c.io.specIssue.bits.rs2WbLocation, 0)
  expect(c.io.specIssue.bits.rs2CycleNum, 0) 	
	expect(c.io.specIssue.bits.rs2IsSpec, 1)
	expect(c.io.specIssue.valid, 1)

	println("// Test3b")
	
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

	println("// Test3c")
	
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

	println("// Test3d")
	
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

	println("// Test3e")
	
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

	println("// Test4 - Adding test to completely fill IQ")
	println("// write back a value and issue one instruction at a time")
	
	poke(c.io.robWb.entry_s1(0).operand, 4)
	poke(c.io.robWb.entry_s1(0).valid, 0)
	
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
	
	println("// Test4a")
	
	for (i <- 0 to 7) {
			poke(c.io.issuedPrev2(i).valid,0)
	}	
	
	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.entry_s1(0).operand, 5)
	poke(c.io.robWb.entry_s1(0).valid, 1)

	step(1)
	
	expect(c.io.currentLen, 16)
	expect(c.io.issuedEntry.valid, 0)
  
	println("// Test4b")
	
	poke(c.io.robWb.entry_s1(0).operand, 4)
	poke(c.io.robWb.entry_s1(0).valid, 0)
	
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
	
	println("// Test4c")
	
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

	println("// Test5 - 1st issue 2nd instruction, followed by 1st")
	
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

	println("// Test5b")

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

	println("// Test5c")
	
	poke(c.io.issuedPrev2(7).valid,1)
	poke(c.io.issuedPrev2(7).bits.tag,1)
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

	println("// Test5c")
	
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

	println("// Test6")
	println("//	 - check if non filled issue queue are generating a valid signal")
	poke(c.io.robWb.entry_s1(0).operand, 0)
	poke(c.io.robWb.entry_s1(0).valid, 1)

	step(1)
	
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 0)

	println("// Test7 - if incoming inst takes value from WB")
	for (i <- 0 to 7) {
		poke(c.io.issuedPrev2(i).valid,0)
	}
	poke(c.io.newEntry(0).valid, 1)
	poke(c.io.newEntry(0).bits.tag, 0)
	poke(c.io.newEntry(0).bits.rs1Val.valid, 0)
	poke(c.io.newEntry(0).bits.rs2Val.valid, 1)
	poke(c.io.newEntry(0).bits.rs1Rename, 1)
	
	for (i <- 1 to 3) {
		poke(c.io.newEntry(i).valid, 1)
		poke(c.io.newEntry(i).bits.tag, i)
		poke(c.io.newEntry(i).bits.rs1Val.valid, 1)
		poke(c.io.newEntry(i).bits.rs2Val.valid, 1)
	}
	poke(c.io.robWb.entry_s1(0).operand, 1)
	poke(c.io.robWb.entry_s1(0).valid, 1)

	step(1)
	
	expect(c.io.currentLen, 4)
	expect(c.io.issuedEntry.valid, 0)
	
	println("// Test7a - instruction should be issued in this cycle")

	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.entry_s1(0).valid, 1)
	poke(c.io.robWb.entry_s1(1).valid, 1)
	poke(c.io.robWb.entry_s1(2).valid, 1)
	poke(c.io.robWb.entry_s1(3).valid, 1)

	step(1)
	expect(c.io.currentLen, 3)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 0)

	println("// Test7b - instruction should be issued in this cycle")

	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.entry_s1(0).valid, 1)
	poke(c.io.robWb.entry_s1(1).valid, 1)
	poke(c.io.robWb.entry_s1(2).valid, 1)
	poke(c.io.robWb.entry_s1(3).valid, 1)

	step(1)
	
	expect(c.io.currentLen, 2)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 1)

	println("// Test7c - instruction should be issued in this cycle")

	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.entry_s1(0).valid, 1)
	poke(c.io.robWb.entry_s1(1).valid, 1)
	poke(c.io.robWb.entry_s1(2).valid, 1)
	poke(c.io.robWb.entry_s1(3).valid, 1)

	step(1)
	
	expect(c.io.currentLen, 1)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 2)

	println("// Test7c - instruction should be issued in this cycle")

	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.entry_s1(0).valid, 1)
	poke(c.io.robWb.entry_s1(1).valid, 1)
	poke(c.io.robWb.entry_s1(2).valid, 1)
	poke(c.io.robWb.entry_s1(3).valid, 1)

	step(1)
	
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 1)
	expect(c.io.issuedEntry.bits.tag, 3)

	println("// Test7d - instruction should be issued in this cycle")

	poke(c.io.newEntry(0).valid, 0)
	poke(c.io.newEntry(1).valid, 0)
	poke(c.io.newEntry(2).valid, 0)
	poke(c.io.newEntry(3).valid, 0)
	poke(c.io.robWb.entry_s1(0).valid, 1)
	poke(c.io.robWb.entry_s1(1).valid, 1)
	poke(c.io.robWb.entry_s1(2).valid, 1)
	poke(c.io.robWb.entry_s1(3).valid, 1)

	step(1)
	
	expect(c.io.currentLen, 0)
	expect(c.io.issuedEntry.valid, 0)
}

class IssueQueueGenerator extends TestGenerator {
  def genMod(): Module = Module(new IssueQueue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueQueueTests(c.asInstanceOf[IssueQueue])).asInstanceOf[Tester[T]]
}
