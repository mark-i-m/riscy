package riscy

import Chisel._

class IssuedInst extends Bundle {
	val inst = {Valid (new ROBEntry)}
	val iqNum = UInt(OUTPUT,2)
}

class AddrBufEntry extends Bundle {
	val robLoc = UInt(OUTPUT, 6)
	val st_nld = Bool(OUTPUT)
	val funct3 = UInt(OUTPUT,3)
	val rs1Rename = UInt(OUTPUT, 6)
  val rs2Rename = UInt(OUTPUT, 6)
  val rs1Val = Valid(UInt(OUTPUT, 64))
  val rs2Val = Valid(UInt(OUTPUT, 64))	
}

class IqArbiter extends Module {
	val io = new Bundle {
		val inst = Vec.fill(4) {Valid (new ROBEntry).flip}
		val iqLen = Vec.fill(4) { UInt(INPUT, 5)}
		val addrBufLen = UInt(INPUT, 5)
		// Instruction issue to addrress Queue
		val allocIQ = Vec.fill(4) (new IssuedInst)
		// Addrress buf entry and load store info
		val addrBuf = Vec.fill(4) {Valid (new AddrBufEntry)}
		val stall = Bool(OUTPUT)
	}
	// Logic to generate initial stalls in design
	// Currently stalling the processor if there are less than 4 
	// entries available all 4 Issue Queues combined
	// to make iqLen 7(max 64) width this new variable as scala does not 
	val iqLen7W = Vec.fill(4) {UInt(width = 7)}
	iqLen7W := io.iqLen
	val totalLen = iqLen7W(0) + iqLen7W(1) + iqLen7W(2) + iqLen7W(3)
	when ((io.addrBufLen > UInt(0x1b)) ||
	      (totalLen > UInt(0x3c))) {
		      io.stall := Bool(true)
	      } .otherwise {
		      io.stall := Bool(false)
	      }
	
	//To track length of the queue once instructions have been assigned to queues
	val min1 = UInt(width = 2)
	val min2 = UInt(width = 2)
	val max1 = UInt(width = 2)
	val max2 = UInt(width = 2)
	val finalMin = Vec.fill(4) {UInt(width = 2)}
	val minDiff = Vec.fill(3) {UInt(width = 4)}
	
	// below logic is sorting all four iq lengths
	// I have written nested loop but it was not working
	// also we have to follow this method of sorting as
	// we can not increment queue lengths on the go without losing a cycle
	when (io.iqLen(0) <= io.iqLen(1)) {
		min1 := UInt(0)
		max1 := UInt(1)
	} .otherwise {
		min1 := UInt(1)
		max1 := UInt(0)
	}
	when (io.iqLen(2) <= io.iqLen(3)) {
		min2 := UInt(2)
		max2 := UInt(3)
	} .otherwise {
		min2 := UInt(3)
		max2 := UInt(2)
	}
	when ((io.iqLen(min1) <= io.iqLen(min2)) && (io.iqLen(max1) <= io.iqLen(min2))) {
		finalMin(0) := min1
		finalMin(1) := max1
		finalMin(2) := min2
		finalMin(3) := max2
	} .elsewhen ((io.iqLen(min1) <= io.iqLen(min2)) && !(io.iqLen(max1) <= io.iqLen(min2)) && (io.iqLen(max1) <= io.iqLen(max2))) {
		finalMin(0) := min1
		finalMin(1) := min2
		finalMin(2) := max1
		finalMin(3) := max2
	} .elsewhen ((io.iqLen(min1) <= io.iqLen(min2)) && !(io.iqLen(max1) <= io.iqLen(min2)) && !(io.iqLen(max1) <= io.iqLen(max2))) {
		finalMin(0) := min1
		finalMin(1) := min2
		finalMin(2) := max2
		finalMin(3) := max1
	} .elsewhen (!(io.iqLen(min1) <= io.iqLen(min2)) && (io.iqLen(max2) <= io.iqLen(min1))) {
		finalMin(0) := min2
		finalMin(1) := max2
		finalMin(2) := min1
		finalMin(3) := max1
	} .elsewhen (!(io.iqLen(min1) <= io.iqLen(min2)) && !(io.iqLen(max2) <= io.iqLen(min1)) && (io.iqLen(max1) <= io.iqLen(max2))) {
		finalMin(0) := min2
		finalMin(1) := min1
		finalMin(2) := max1
		finalMin(3) := max2
	} .otherwise {
		finalMin(0) := min2
		finalMin(1) := min1
		finalMin(2) := max2
		finalMin(3) := max1
	}

  for (i <- 0 until 3) {
		minDiff(i) := io.iqLen(finalMin(i+1)) - io.iqLen(finalMin(i))
	}
	
	// pipeline the stall

	//val pipeStall = Reg (next = io.stall)


	for (i <- 0 until 4) {
		when (!io.stall) {
			io.allocIQ(i).inst.bits := io.inst(i).bits
			io.allocIQ(i).inst.valid := io.inst(i).valid
			
			// Logic to issue instructions to different iqs
			// if there is a stall signal set, top issue module
			// should not issue any instructions to any iqs
			when (UInt(i) <= minDiff(0)) {
				io.allocIQ(i).iqNum := finalMin(0)
			} .elsewhen (UInt(i) <= (minDiff(1) + minDiff(0) + UInt(1))) {
				io.allocIQ(i).iqNum := finalMin(1)
			} .elsewhen (UInt(i) <= (minDiff(2) + minDiff(1) + minDiff(0) + UInt(2))) {
				io.allocIQ(i).iqNum := finalMin(2)
			} .otherwise {
				io.allocIQ(i).iqNum := finalMin(3)
			}
	
			// Logic to generate the entry for LS Buffer
			when ((io.inst(i).valid === Bool(true)) && 
						(io.inst(i).bits.isLd === Bool(true))) {
				io.addrBuf(i).valid := Bool(true)
				io.addrBuf(i).bits.st_nld := Bool(false)
			} .elsewhen ((io.inst(i).valid === Bool(true)) && 
									 (io.inst(i).bits.isSt === Bool(true))) {
				io.addrBuf(i).valid := Bool(true)
				io.addrBuf(i).bits.st_nld := Bool(true)
			} .otherwise {
				io.addrBuf(i).valid := Bool(false)
				io.addrBuf(i).bits.st_nld := Bool(false)
			}
			io.addrBuf(i).bits.robLoc 		:= io.inst(i).bits.tag
			io.addrBuf(i).bits.funct3 		:= io.inst(i).bits.funct3
			io.addrBuf(i).bits.rs1Rename 	:= io.inst(i).bits.rs1Rename
			io.addrBuf(i).bits.rs1Val 		:= io.inst(i).bits.rs1Val
			io.addrBuf(i).bits.rs2Rename 	:= io.inst(i).bits.rs2Rename
			io.addrBuf(i).bits.rs2Val 		:= io.inst(i).bits.rs2Val
		} .otherwise {

			// Setting all values to false is stall is set
			io.allocIQ(i).inst.bits := io.inst(i).bits
			io.allocIQ(i).inst.valid := Bool(false)
			io.allocIQ(i).iqNum := finalMin(0)
			io.addrBuf(i).valid := Bool(false)
			io.addrBuf(i).bits.st_nld := Bool(false)
			io.addrBuf(i).bits.robLoc 		:= io.inst(i).bits.tag
			io.addrBuf(i).bits.funct3 		:= io.inst(i).bits.funct3
			io.addrBuf(i).bits.rs1Rename 	:= io.inst(i).bits.rs1Rename
			io.addrBuf(i).bits.rs1Val 		:= io.inst(i).bits.rs1Val
			io.addrBuf(i).bits.rs2Rename 	:= io.inst(i).bits.rs2Rename
			io.addrBuf(i).bits.rs2Val 		:= io.inst(i).bits.rs2Val
		}
	}
}

class IqArbiterTests(c: IqArbiter) extends Tester(c) {
	
	// Test - 1  to check if all correct instructions are getting assigned
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  	poke(c.io.iqLen(0), 0x4)
	poke(c.io.iqLen(1), 0x4)
  	poke(c.io.iqLen(2), 0x4)
	poke(c.io.iqLen(3), 0x4)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.allocIQ(0).iqNum, 0x0)
	expect(c.io.allocIQ(1).iqNum, 0x1)
	expect(c.io.allocIQ(2).iqNum, 0x2)
	expect(c.io.allocIQ(3).iqNum, 0x3)
	expect(c.io.allocIQ(0).inst.valid, 0x1)
	expect(c.io.allocIQ(1).inst.valid, 0x1)
	expect(c.io.allocIQ(2).inst.valid, 0x1)
	expect(c.io.allocIQ(3).inst.valid, 0x1)
	expect(c.io.stall, 0x0)

	// Test - 2a check if stall is getting generated if all iqs are full
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  poke(c.io.iqLen(0), 0xf)
	poke(c.io.iqLen(1), 0xf)
  poke(c.io.iqLen(2), 0xf)
	poke(c.io.iqLen(3), 0xf)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.stall, 0x0)

	// Test - 2b check if stall is getting generated if all iqs are full
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  poke(c.io.iqLen(0), 0x10)
	poke(c.io.iqLen(1), 0x10)
  poke(c.io.iqLen(2), 0x10)
	poke(c.io.iqLen(3), 0x10)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.stall, 0x1)

	// Test - 2c check if stall is getting generated if all iqs are full
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  poke(c.io.iqLen(0), 0xf)
	poke(c.io.iqLen(1), 0xf)
  poke(c.io.iqLen(2), 0xf)
	poke(c.io.iqLen(3), 0x10)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.stall, 0x1)

	// Test - 3 check if stall is getting generated if all lsq is full
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  	poke(c.io.iqLen(0), 0x1)
	poke(c.io.iqLen(1), 0x1)
  	poke(c.io.iqLen(2), 0x1)
	poke(c.io.iqLen(3), 0x1)
	poke(c.io.addrBufLen, 0x1c)

	step(1)

	expect(c.io.stall, 0x1)

	// Test - 4  to check if all correct instructions are getting assigned
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  	poke(c.io.iqLen(0), 0x2)
	poke(c.io.iqLen(1), 0x5)
  	poke(c.io.iqLen(2), 0x6)
	poke(c.io.iqLen(3), 0x7)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.allocIQ(0).iqNum, 0x0)
	expect(c.io.allocIQ(1).iqNum, 0x0)
	expect(c.io.allocIQ(2).iqNum, 0x0)
	expect(c.io.allocIQ(3).iqNum, 0x0)
	expect(c.io.allocIQ(0).inst.valid, 0x1)
	expect(c.io.allocIQ(1).inst.valid, 0x1)
	expect(c.io.allocIQ(2).inst.valid, 0x1)
	expect(c.io.allocIQ(3).inst.valid, 0x1)
	expect(c.io.stall, 0x0)

	// Test - 5  to check if all correct instructions are getting assigned
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  	poke(c.io.iqLen(0), 0x9)
	poke(c.io.iqLen(1), 0x6)
  	poke(c.io.iqLen(2), 0x2)
	poke(c.io.iqLen(3), 0xf)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.allocIQ(0).iqNum, 0x2)
	expect(c.io.allocIQ(1).iqNum, 0x2)
	expect(c.io.allocIQ(2).iqNum, 0x2)
	expect(c.io.allocIQ(3).iqNum, 0x2)
	expect(c.io.allocIQ(0).inst.valid, 0x1)
	expect(c.io.allocIQ(1).inst.valid, 0x1)
	expect(c.io.allocIQ(2).inst.valid, 0x1)
	expect(c.io.allocIQ(3).inst.valid, 0x1)
	expect(c.io.stall, 0x0)

	// Test - 6  to check if all correct instructions are getting assigned
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
	poke(c.io.inst(0).bits.isLd, 1)
	poke(c.io.inst(1).bits.isSt, 1)
	poke(c.io.inst(2).bits.isLd, 1)
	poke(c.io.inst(3).bits.isSt, 0)
  	poke(c.io.iqLen(0), 0x9)
	poke(c.io.iqLen(1), 0x6)
  	poke(c.io.iqLen(2), 0x2)
	poke(c.io.iqLen(3), 0xf)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.allocIQ(0).iqNum, 0x2)
	expect(c.io.allocIQ(1).iqNum, 0x2)
	expect(c.io.allocIQ(2).iqNum, 0x2)
	expect(c.io.allocIQ(3).iqNum, 0x2)
	expect(c.io.addrBuf(0).bits.st_nld, 0)
	expect(c.io.addrBuf(1).bits.st_nld, 1)
	expect(c.io.addrBuf(2).bits.st_nld, 0)
	expect(c.io.addrBuf(0).valid, 1)
	expect(c.io.addrBuf(1).valid, 1)
	expect(c.io.addrBuf(2).valid, 1)
	expect(c.io.addrBuf(3).valid, 0)
	expect(c.io.allocIQ(0).inst.valid, 0x1)
	expect(c.io.allocIQ(1).inst.valid, 0x1)
	expect(c.io.allocIQ(2).inst.valid, 0x1)
	expect(c.io.allocIQ(3).inst.valid, 0x1)
	expect(c.io.stall, 0x0)

	// Test - 7  to check if all correct instructions are getting assigned
	poke(c.io.inst(0).valid, 1)
	poke(c.io.inst(1).valid, 1)
	poke(c.io.inst(2).valid, 1)
	poke(c.io.inst(3).valid, 1)
  	poke(c.io.iqLen(0), 0x4)
	poke(c.io.iqLen(1), 0x5)
  	poke(c.io.iqLen(2), 0x6)
	poke(c.io.iqLen(3), 0x7)
	poke(c.io.addrBufLen, 0x7)

	step(1)

	expect(c.io.allocIQ(0).iqNum, 0x0)
	expect(c.io.allocIQ(1).iqNum, 0x0)
	expect(c.io.allocIQ(2).iqNum, 0x1)
	expect(c.io.allocIQ(3).iqNum, 0x1)
	expect(c.io.allocIQ(0).inst.valid, 0x1)
	expect(c.io.allocIQ(1).inst.valid, 0x1)
	expect(c.io.allocIQ(2).inst.valid, 0x1)
	expect(c.io.allocIQ(3).inst.valid, 0x1)
	expect(c.io.stall, 0x0)

}

class IqArbiterGenerator extends TestGenerator {
  def genMod(): Module = Module(new IqArbiter())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IqArbiterTests(c.asInstanceOf[IqArbiter])).asInstanceOf[Tester[T]]
}
