package riscy

import Chisel._

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Valid(new ROBEntry).asInput
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 4)
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Module (new ShiftRegPP(() => new  ROBEntry))}
//	for (i <- 0 until 16) {
//		val iqueue(i) = Module (new ShiftRegPP(() => new  ROBEntry))
//	}

//	iqueue(io.currentLen).io.wen := io.newEntry.valid
//	iqueue(io.currentLen).io.data := io.newEntry.bits

	// TODO: issuedEntry := first allReady instruction
	// TODO: pop when woken up

//	val allReady = Vec.tabulate(16) {
//			// TODO: speculative wakeup
//		i => iqueue(i).io.value.rs1Val.valid && iqueue(i).io.value.rs2Val.valid
//	}
}

class IssueQueueTests(c: IssueQueue) extends Tester(c) {
  println("TODO")
}

class IssueQueueGenerator extends TestGenerator {
  def genMod(): Module = Module(new IssueQueue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueQueueTests(c.asInstanceOf[IssueQueue])).asInstanceOf[Tester[T]]
}
