package riscy

import Chisel._

class IssueQueue extends Module {
	val io = new Bundle {
		val newEntry = Valid(new ROBEntry).asInput
		val issuedEntry = Valid(new ROBEntry).asOutput
		val currentLen = UInt(OUTPUT, 4)
	}

	//val eachEntry = Module ( new ShiftRegPP(() => new  ROBEntry))
	val iqueue = Vec.fill(16) {Reg(outType = Valid(new ROBEntry))}
//	for (i <- 0 until 16) {
//		val iqueue(i) = Module (new ShiftRegPP(() => new  ROBEntry))
//	}

	val (len, wrap) = Counter(io.newEntry.valid, 16)
	iqueue(len).valid := io.newEntry.valid
	iqueue(len).bits := io.newEntry.bits

	io.currentLen := len


	// TODO: issuedEntry := first allReady instruction
	// TODO: pop when woken up

	val allReady = Vec.tabulate(16) {
			// TODO: speculative wakeup
		i => iqueue(i).valid && iqueue(i).bits.rs1Val.valid && iqueue(i).bits.rs2Val.valid
	}

	for (i <- 0 until 16) {
		when (allReady(i)) {
			io.issuedEntry.valid := Bool(true)
			io.issuedEntry.bits := iqueue(i).bits
		} .otherwise {
			io.issuedEntry.valid := Bool(false)
			io.issuedEntry.bits := io.newEntry.bits
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
