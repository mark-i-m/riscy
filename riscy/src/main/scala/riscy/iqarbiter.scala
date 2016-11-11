package riscy

import Chisel._

class IssuedInst extends Bundle {
	val inst = {Valid (new AllocROB())}
	val iqNum = UInt(OUTPUT,2)
}

class AddBufEntry extends Bundle {
	val robLoc = UInt(OUTPUT, 6)
	val lsType = Bool(OUTPUT)
}

class IqArbiter extends Module {
	val io = new Bundle {
		val inst = Vec.fill(4) {Valid (new AllocROB()).flip}
		val iqLen = Vec.fill(4) { UInt(INPUT, 4)}
		val addBufLen = UInt(INPUT, 5)
		// Instruction issue to address Queue
		val allocIQ = Vec.fill(4) (new IssuedInst())
		// Address buf entry and load store info
		val addBuf = Vec.fill(4) {Valid (new AddBufEntry())}
		val stall = Bool(OUTPUT)
	}
	// Logic to generate initial stalls in design
	// Currently stalling the processor if there are less than 4 
	// entries available all 4 Issue Queues combined
	val totalLen = io.iqLen(0) + io.iqLen(1) + io.iqLen(2) + io.iqLen(3)
	when ((io.addBufLen === UInt(31)) ||
	      (totalLen > UInt(56))) {
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
	when ((min1 <= min2) && (max1 <= min2)) {
		finalMin(0) := min1
		finalMin(1) := max1
		finalMin(2) := min2
		finalMin(3) := max2
	} .elsewhen ((min1 <= min2) && !(max1 <= min2) && (max1 <= max2)) {
		finalMin(0) := min1
		finalMin(1) := min2
		finalMin(2) := max1
		finalMin(3) := max2
	} .elsewhen ((min1 <= min2) && !(max1 <= min2) && !(max1 <= max2)) {
		finalMin(0) := min1
		finalMin(1) := min2
		finalMin(2) := max2
		finalMin(3) := max1
	} .elsewhen (!(min1 <= min2) && (max2 <= min1)) {
		finalMin(0) := min2
		finalMin(1) := max2
		finalMin(2) := min1
		finalMin(3) := max1
	} .elsewhen (!(min1 <= min2) && !(max2 <= min1) && (max1 <= max2)) {
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
		minDiff(i) := io.iqLen(finalMin(i+1)) - io.iqLen(finalMin(0))
	}

	for (i <- 0 until 4) {
		io.allocIQ(i).inst.bits := io.inst(i).bits
		io.allocIQ(i).inst.valid := io.inst(i).valid
		
		// Logic to issue instructions to different iqs
		// if there is a stall signal set, top issue module
		// should not issue any instructions to any iqs
		when (UInt(i) < minDiff(0)) {
			io.allocIQ(i).iqNum := finalMin(0)
		} .elsewhen (UInt(i) < minDiff(1)) {
			io.allocIQ(i).iqNum := finalMin(1)
		} .elsewhen (UInt(i) < minDiff(2)) {
			io.allocIQ(i).iqNum := finalMin(2)
		} .otherwise {
			io.allocIQ(i).iqNum := finalMin(3)
		}

		// Logic to generate the entry for LS Buffer
		when (io.inst(i).bits.op === UInt(0x00)) {
			io.addBuf(i).valid := Bool(true)
			io.addBuf(i).bits.robLoc := io.inst(i).bits.entry
			io.addBuf(i).bits.lsType := Bool(true)
		} .elsewhen (io.inst(i).bits.op === UInt(0x08)) {
			io.addBuf(i).valid := Bool(true)
			io.addBuf(i).bits.robLoc := io.inst(i).bits.entry
			io.addBuf(i).bits.lsType := Bool(false)
		} .otherwise {
			io.addBuf(i).valid := Bool(false)
			io.addBuf(i).bits.robLoc := io.inst(i).bits.entry
			io.addBuf(i).bits.lsType := Bool(false)
		}
	}
}

class IqArbiterTests(c: IqArbiter) extends Tester(c) {
  println("TODO")
}

class IqArbiterGenerator extends TestGenerator {
  def genMod(): Module = Module(new IqArbiter())
  def genTest[T <: Module](c: T): Tester[T] =
    (new IqArbiterTests(c.asInstanceOf[IqArbiter])).asInstanceOf[Tester[T]]
}
