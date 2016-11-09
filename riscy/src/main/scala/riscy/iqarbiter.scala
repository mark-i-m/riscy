package riscy

import Chisel._

class MinFinder(val n: Vec[UInt]) {
	def compute(): UInt = {
	val min1 = UInt(0)
	val min2 = UInt(0)
	val min = UInt(0)
	when (n(0) <= n(1)) {min1 := UInt(0)} .otherwise {min1 := UInt(1)}
	when (n(2) <= n(3)) {min2 := UInt(2)} .otherwise {min2 := UInt(3)}
	when (min1 <= min2) {min := min1} .otherwise {min := min2}
	min
	}
}

class IssuedInst extends Bundle {
	val inst = new AllocROB()
	val iqNum = UInt(OUTPUT, 2)
}

class IqArbiter extends Module {
	val io = new Bundle {
		val inst = Vec.fill(4) {Valid (new AllocROB()).flip}
		val iqLen = Vec.fill(4) { UInt(INPUT, 4)}
		val addressqLen = UInt(INPUT, 5)
		val allocIQ = Vec.fill(4) (new IssuedInst()) 
		val addressqEntry = Vec.fill(4) {Valid (UInt (OUTPUT, 6))}
		val stall = Bool(OUTPUT)
	}
	// Logic to generate initial stalls in design
	when (io.addressqLen === UInt(31) ||
	      (io.iqLen(0) === UInt(15) &&
	       io.iqLen(1) === UInt(15) &&
	       io.iqLen(2) === UInt(15) &&
	       io.iqLen(3) === UInt(15))) {
		      io.stall := Bool(true)
	      } .otherwise {
		      io.stall := Bool(false)
	      }
	
	//To track length of the queue once instructions have been assigned to queues
	val iqLenIncr = Vec.tabulate(4) {i => {io.iqLen}}
	
	for (i <- 0 until 4) {
		val issueQueueNum = new MinFinder(iqLenIncr(i))
		io.allocIQ(i).inst := io.inst(i).bits
		io.allocIQ(i).iqNum := UInt(3) 
//			if (i == 0) {
//				iqLenIncr(i)(issuedQueue) := io.iqLen(issuedQueue) + UInt(1)
//			} else {
//				iqLenIncr(i)(issuedQueue) := iqLenIncr(i-1)(issuedQueue) + UInt(1)
//			}
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
