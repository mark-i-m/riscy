package riscy

import Chisel._

class Issue extends Module {
	val io = new Bundle {
		val inst = Vec.fill(4) {Valid (new ROBEntry).flip}
		// Values from rob_wb with valid signal
		val robWb = new RobWbStore(6).flip
		val addBufLen = UInt(INPUT, 5)
 		val stall = Bool(OUTPUT)
		val issuedEntry = Vec.fill(4) {Valid(new ROBEntry).asOutput}
		// Address buf entry and load store info
		val addBuf = Vec.fill(4) {Valid (new AddBufEntry)}
	}

	val arbiter = Module (new IqArbiter())
	val issueQ0 = Module (new IssueQueue())
	val issueQ1 = Module (new IssueQueue())
	val issueQ2 = Module (new IssueQueue())
	val issueQ3 = Module (new IssueQueue())

  // connections for arbiter
	arbiter.io.inst 			:= io.inst
	arbiter.io.addBufLen	:= io.addBufLen
	arbiter.io.iqLen(0) 	:= issueQ0.io.currentLen
	arbiter.io.iqLen(1) 	:= issueQ1.io.currentLen
	arbiter.io.iqLen(2) 	:= issueQ2.io.currentLen
	arbiter.io.iqLen(3) 	:= issueQ3.io.currentLen
	val allocatedInst 	 	 = arbiter.io.allocIQ
	io.addBuf 						:= arbiter.io.addBuf
	io.stall 							:= arbiter.io.stall

	// calculating instructions per issue queue
	for (i <- 0 until 4) {
		when (allocatedInst(i).inst.valid === Bool(true)) {
			when (allocatedInst(i).iqNum === UInt (0)) {
				issueQ0.io.newEntry(i).valid := Bool(true)
				issueQ0.io.newEntry(i).bits  := allocatedInst(i).inst.bits
				issueQ1.io.newEntry(i).valid := Bool(false)
				issueQ2.io.newEntry(i).valid := Bool(false)
				issueQ3.io.newEntry(i).valid := Bool(false)
			} .elsewhen (allocatedInst(i).iqNum === UInt (1)) {
				issueQ1.io.newEntry(i).valid := Bool(true)
				issueQ1.io.newEntry(i).bits  := allocatedInst(i).inst.bits
				issueQ0.io.newEntry(i).valid := Bool(false)
				issueQ2.io.newEntry(i).valid := Bool(false)
				issueQ3.io.newEntry(i).valid := Bool(false)
			} .elsewhen (allocatedInst(i).iqNum === UInt (2)) {
				issueQ2.io.newEntry(i).valid := Bool(true)
				issueQ2.io.newEntry(i).bits  := allocatedInst(i).inst.bits
				issueQ0.io.newEntry(i).valid := Bool(false)
				issueQ1.io.newEntry(i).valid := Bool(false)
				issueQ3.io.newEntry(i).valid := Bool(false)
			} .otherwise {
				issueQ3.io.newEntry(i).valid := Bool(true)
				issueQ3.io.newEntry(i).bits  := allocatedInst(i).inst.bits
				issueQ0.io.newEntry(i).valid := Bool(false)
				issueQ1.io.newEntry(i).valid := Bool(false)
				issueQ2.io.newEntry(i).valid := Bool(false)
			}
		} .otherwise {
			issueQ0.io.newEntry(i).valid 	:= Bool(false)
			issueQ0.io.newEntry(i).bits 	:= allocatedInst(i).inst.bits
			issueQ1.io.newEntry(i).valid 	:= Bool(false)
			issueQ1.io.newEntry(i).bits 	:= allocatedInst(i).inst.bits
			issueQ2.io.newEntry(i).valid	:= Bool(false)
			issueQ2.io.newEntry(i).bits 	:= allocatedInst(i).inst.bits
			issueQ3.io.newEntry(i).valid 	:= Bool(false)
			issueQ3.io.newEntry(i).bits 	:= allocatedInst(i).inst.bits
		}
	}
	// Providing robWB bypass values to  
	issueQ0.io.robWb			:= io.robWb
	issueQ1.io.robWb			:= io.robWb
	issueQ2.io.robWb			:= io.robWb
	issueQ3.io.robWb			:= io.robWb
  
	// Collecting issued inst from all issue queues
	io.issuedEntry(0) 		:= issueQ0.io.issuedEntry
	io.issuedEntry(1) 		:= issueQ1.io.issuedEntry
	io.issuedEntry(2) 		:= issueQ2.io.issuedEntry
	io.issuedEntry(3) 		:= issueQ3.io.issuedEntry
  
	val issuedInstTag = Vec.fill(4) {UInt(width = 6)}
	issuedInstTag(0) 		:= issueQ0.io.issuedEntry.bits.tag
	issuedInstTag(1) 		:= issueQ1.io.issuedEntry.bits.tag
	issuedInstTag(2) 		:= issueQ2.io.issuedEntry.bits.tag
	issuedInstTag(3) 		:= issueQ3.io.issuedEntry.bits.tag

	val issuedInstValid = Vec.fill(4) {Bool()}
	issuedInstValid(0) 		:= issueQ0.io.issuedEntry.valid
	issuedInstValid(1) 		:= issueQ1.io.issuedEntry.valid
	issuedInstValid(2) 		:= issueQ2.io.issuedEntry.valid
	issuedInstValid(3) 		:= issueQ3.io.issuedEntry.valid

	val pipelinedIssuedInstTag = Vec.tabulate(4) {
    i => Reg(next = issuedInstTag(i))
  }
	val pipelinedIssuedInstValid = Vec.tabulate(4) {
    i => Reg(next = issuedInstValid(i))
  }

  val issuedPrev2 = Vec.fill(8) {Valid(UInt(width = 6))}

	for (i <- 0 until 4) {
		issuedPrev2(i).bits  := issuedInstTag(i)
		issuedPrev2(i).valid := issuedInstValid(i)
	}
	for (i <- 4 until 8) {
		issuedPrev2(i).bits := pipelinedIssuedInstTag(i-4)
		issuedPrev2(i).valid := pipelinedIssuedInstValid(i-4)
	}

	// Providing tags of last 2 instructions to all
	// issue queues for wakeup - TODO
//	issueQ0.io.issuedPrev2	:= issuedPrev2
//	issueQ1.io.issuedPrev2	:= issuedPrev2
//	issueQ2.io.issuedPrev2	:= issuedPrev2
//	issueQ3.io.issuedPrev2	:= issuedPrev2
}

class IssueTests(c: Issue) extends Tester(c) {
  println("TODO")
}

class IssueGenerator extends TestGenerator {
  def genMod(): Module = Module(new Issue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueTests(c.asInstanceOf[Issue])).asInstanceOf[Tester[T]]
}
