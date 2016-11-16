package riscy

import Chisel._

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Vec.fill(4) {Valid(new AllocROB).asInput}
		val totalEntries = UInt(INPUT, 2)
		// Currently just assume that below signals contain insts
		// issued in last 2 cycles as form of ROB entry
		// At top level this structure has to be generated - TODO
		val issuedPrev2 = Vec.fill(8) {Valid(UInt(INPUT, 6))}
		// Values from rob_wb with valid signal
		val robWb = new RobWbStore(6).flip
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 4)
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Reg(outType = new AllocROB)}
	
	//Counter to maintain length of IQ
	val counter = new MultiCounter(16)

	//CAM to update RS1, RS2 values based on ROB_wb
	val wbCamRs1 = Module (new CAM(6, 16, 6))
	val wbCamRs2 = Module (new CAM(6, 16, 6))

	for (i <- 0 until 16) {
		wbCamRs1.io.input_bits(i) := iqueue(i).rs1Val.valid
		wbCamRs2.io.input_bits(i) := iqueue(i).rs2Val.valid
	}
	for (i <- 0 until 6) {
		wbCamRs1.io.compare_bits(i) := io.robWb.operand_s1(i)
		wbCamRs2.io.compare_bits(i) := io.robWb.operand_s1(i)
	}

	// Assigning entries to current issue queue entries
	// New entries are assigned only if valid bit is set
	for (i <- 0 until 4) {
		when (io.newEntry(i).valid) {
			iqueue(counter.value + UInt(i)) := io.newEntry(i).bits
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
	
	val WakeUpRs1 = Vec.fill(16) {Bool()}
	val WakeUpRs2 = Vec.fill(16) {Bool()}

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
					WakeUpRs1(i) === Bool(true) 
				} .otherwise {
					WakeUpRs1(i) === iqueue(j).rs1Val.valid
				}
				when (iqueue(j).rs2Val.valid === Bool(false) && 
				      iqueue(j).rs2Rename === io.issuedPrev2(i).bits) {
					WakeUpRs2(i) === Bool(true) 
				} .otherwise {
					WakeUpRs2(i) === iqueue(j).rs2Val.valid
				}
			}
		}
	}

	val allReady = Vec.tabulate(16) {
		i =>  WakeUpRs1(i) && WakeUpRs2(i)
	}

	// Issuing the oldest ready instruction
	// If none of the instructions are ready queue will give 
	// oldest instrution with valid bit not set
	val issuedNum = PriorityEncoder(allReady)
	val issuedNumOH = UIntToOH(issuedNum)
	io.issuedEntry := MuxLookup(issuedNum, iqueue(0), 
	Array.tabulate(16) {i => UInt (i) -> iqueue(i)})

	// First case is when instruction is issued from issue queue
	// Logic to shift instructions up in the order
	// if ith instruction is issued, we have to shift 
	// from i+1th instruction to 15th instruction one place up
	// shift will happen only if one instruction is issued with valid
	when (io.issuedEntry.valid) {
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
  println("TODO")
}

class IssueQueueGenerator extends TestGenerator {
  def genMod(): Module = Module(new IssueQueue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueQueueTests(c.asInstanceOf[IssueQueue])).asInstanceOf[Tester[T]]
}
