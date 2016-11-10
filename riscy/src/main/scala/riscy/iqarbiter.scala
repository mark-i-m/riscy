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
	val iqLenIncr = Vec.fill(5) {Vec.fill(4) { UInt(INPUT, 4)}}
	iqLenIncr(0) := io.iqLen
	val min1 = UInt(width = 2)
	val min2 = UInt(width = 2)
	
	for (i <- 0 until 4) {
		io.allocIQ(i).inst.bits := io.inst(i).bits
		io.allocIQ(i).inst.valid := io.inst(i).valid
//		when (iqLenIncr(i)(0) <= iqLenIncr(i)(1)) {min1 := UInt(0)} .otherwise {min1 := UInt(1)}
//		when (iqLenIncr(i)(2) <= iqLenIncr(i)(3)) {min2 := UInt(2)} .otherwise {min2 := UInt(3)}
//		when (min1 <= min2) {io.allocIQ(i).iqNum := min1} .otherwise {io.allocIQ(i).iqNum := min2}	 		
		iqLenIncr(i+1) := iqLenIncr(i)
		iqLenIncr(i+1)(io.allocIQ(i).iqNum) := iqLenIncr(i)(io.allocIQ(i).iqNum) + UInt(1)
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
