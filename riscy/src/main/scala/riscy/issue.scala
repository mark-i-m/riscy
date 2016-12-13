package riscy

import Chisel._

class IssuedPrev2Inst extends Bundle {
	val tag = UInt(OUTPUT, 6)
	val isLd = Bool(OUTPUT)
}
class Issue extends Module {
  val io = new Bundle {
    // ROB entries
    val inst = Vec.fill(4) {Valid (new ROBEntry).flip} // Input

    // Values from rob_wb with valid signal
    val robWb = new RobWbStore(6).flip // Input
    val addrBufLen = UInt(INPUT, 5)
    val stall = Bool(OUTPUT)
		val in_stall = Bool(INPUT)
    val issuedEntry = Vec.fill(4) {Valid(new ROBEntry).asOutput}
		val specIssue = Vec.fill(4) {Valid(new SpeculativeIssue).asOutput}

    // Addrress buf entry and load store info
    val addrBuf = Vec.fill(4) {Valid (new AddrBufEntry)} // Output

		// era for misspeculation handling
		val era = UInt(INPUT, 7)
  }

  val arbiter = Module (new IqArbiter())
  val issueQ0 = Module (new IssueQueue())
  val issueQ1 = Module (new IssueQueue())
  val issueQ2 = Module (new IssueQueue())
  val issueQ3 = Module (new IssueQueue())

  // connections for arbiter
  arbiter.io.inst         := io.inst
  arbiter.io.addrBufLen		:= io.addrBufLen
  arbiter.io.iqLen(0)     := issueQ0.io.currentLen
  arbiter.io.iqLen(1)     := issueQ1.io.currentLen
  arbiter.io.iqLen(2)     := issueQ2.io.currentLen
  arbiter.io.iqLen(3)     := issueQ3.io.currentLen

  val allocatedInst        = arbiter.io.allocIQ
  io.addrBuf             	:= arbiter.io.addrBuf
  io.stall                := arbiter.io.stall
	arbiter.io.in_stall			:= io.in_stall

  // calculating instructions per issue queue
  for (i <- 0 until 4) {
    when (allocatedInst(i).inst.valid === Bool(true)) {
      when (allocatedInst(i).iqNum === UInt (0x0)) {
        issueQ0.io.newEntry(i).valid := Bool(true)
        issueQ0.io.newEntry(i).bits  := allocatedInst(i).inst.bits
        issueQ1.io.newEntry(i).valid := Bool(false)
        issueQ2.io.newEntry(i).valid := Bool(false)
        issueQ3.io.newEntry(i).valid := Bool(false)
      } .elsewhen (allocatedInst(i).iqNum === UInt (0x1)) {
        issueQ1.io.newEntry(i).valid := Bool(true)
        issueQ1.io.newEntry(i).bits  := allocatedInst(i).inst.bits
        issueQ0.io.newEntry(i).valid := Bool(false)
        issueQ2.io.newEntry(i).valid := Bool(false)
        issueQ3.io.newEntry(i).valid := Bool(false)
      } .elsewhen (allocatedInst(i).iqNum === UInt (0x2)) {
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
      issueQ0.io.newEntry(i).valid    := Bool(false)
      issueQ0.io.newEntry(i).bits     := allocatedInst(i).inst.bits
      issueQ1.io.newEntry(i).valid    := Bool(false)
      issueQ1.io.newEntry(i).bits     := allocatedInst(i).inst.bits
      issueQ2.io.newEntry(i).valid    := Bool(false)
      issueQ2.io.newEntry(i).bits     := allocatedInst(i).inst.bits
      issueQ3.io.newEntry(i).valid    := Bool(false)
      issueQ3.io.newEntry(i).bits     := allocatedInst(i).inst.bits
    }
  }

  // Providing robWB bypass values to issue queues 
  issueQ0.io.robWb            := io.robWb
  issueQ1.io.robWb            := io.robWb
  issueQ2.io.robWb            := io.robWb
  issueQ3.io.robWb            := io.robWb

	issueQ0.io.era         := io.era
  issueQ1.io.era         := io.era
  issueQ2.io.era         := io.era
  issueQ3.io.era         := io.era

  // Collecting issued inst from all issue queues
  io.issuedEntry(0)       := issueQ0.io.issuedEntry
  io.issuedEntry(1)       := issueQ1.io.issuedEntry
  io.issuedEntry(2)       := issueQ2.io.issuedEntry
  io.issuedEntry(3)       := issueQ3.io.issuedEntry

	io.specIssue(0)       := issueQ0.io.specIssue
  io.specIssue(1)       := issueQ1.io.specIssue
  io.specIssue(2)       := issueQ2.io.specIssue
  io.specIssue(3)       := issueQ3.io.specIssue

  val issuedInstTag = Vec.fill(4) {UInt(width = 6)}
  issuedInstTag(0)        := issueQ0.io.issuedEntry.bits.tag
  issuedInstTag(1)        := issueQ1.io.issuedEntry.bits.tag
  issuedInstTag(2)        := issueQ2.io.issuedEntry.bits.tag
  issuedInstTag(3)        := issueQ3.io.issuedEntry.bits.tag

	val issuedInstIsLd = Vec.fill(4) {Bool()}
  issuedInstIsLd(0)        := issueQ0.io.issuedEntry.bits.isLd
  issuedInstIsLd(1)        := issueQ1.io.issuedEntry.bits.isLd
  issuedInstIsLd(2)        := issueQ2.io.issuedEntry.bits.isLd
  issuedInstIsLd(3)        := issueQ3.io.issuedEntry.bits.isLd

  val issuedInstValid = Vec.fill(4) {Bool()}
  issuedInstValid(0)      := issueQ0.io.issuedEntry.valid
  issuedInstValid(1)      := issueQ1.io.issuedEntry.valid
  issuedInstValid(2)      := issueQ2.io.issuedEntry.valid
  issuedInstValid(3)      := issueQ3.io.issuedEntry.valid

  val pipelinedIssuedInstTag = Vec.tabulate(4) {
    i => Reg(next = issuedInstTag(i))
  }
  val pipelinedIssuedInstValid = Vec.tabulate(4) {
    i => Reg(next = issuedInstValid(i))
  }
	val pipelinedIssuedInstIsLd = Vec.tabulate(4) {
    i => Reg(next = issuedInstIsLd(i))
  }

  val issuedPrev2 	= Vec.fill(8) {Valid(new IssuedPrev2Inst())}

  for (i <- 0 until 4) {
    issuedPrev2(i).bits.tag  := issuedInstTag(i)
		issuedPrev2(i).bits.isLd  := issuedInstIsLd(i)
    issuedPrev2(i).valid := issuedInstValid(i)
  }
  for (i <- 4 until 8) {
    issuedPrev2(i).bits.tag := pipelinedIssuedInstTag(i-4)
		issuedPrev2(i).bits.isLd := pipelinedIssuedInstIsLd(i-4)
    issuedPrev2(i).valid := pipelinedIssuedInstValid(i-4)
  }

  // Providing tags of last 2 instructions to all
  // issue queues for wakeup 
  issueQ0.io.issuedPrev2  := issuedPrev2
  issueQ1.io.issuedPrev2  := issuedPrev2
  issueQ2.io.issuedPrev2  := issuedPrev2
  issueQ3.io.issuedPrev2  := issuedPrev2
}

class IssueTests(c: Issue) extends Tester(c) {
  println("TODO")
}

class IssueGenerator extends TestGenerator {
  def genMod(): Module = Module(new Issue())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IssueTests(c.asInstanceOf[Issue])).asInstanceOf[Tester[T]]
}
