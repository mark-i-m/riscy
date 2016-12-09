package riscy

import Chisel._
import scala.language.reflectiveCalls

class DCacheStReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = UInt(INPUT, 64)
  val size = UInt(INPUT, 3)
}

class DCacheLdReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = Valid(UInt(OUTPUT, 64)).asOutput
}

class NBDCache extends Module {
  val io = new Bundle {
    val stReq = Vec(2, new DCacheStReq)
    val ldReq = new DCacheLdReq
    val memStAddrPort = Vec(2, Valid(UInt(OUTPUT,64).asOutput))
    val memStData = Vec(2, UInt(OUTPUT,64))
    val memStSize = Vec(2, UInt(OUTPUT,3))
    val memLdAddrPort = Valid(UInt(OUTPUT,64)).asOutput
    val memLdData = Valid(UInt(INPUT,8 * 64)).asInput
		val ldStall = Bool(OUTPUT)
		val stall = Bool(OUTPUT)

  }
	
	val missCount = Reg(init=UInt(0, width = 3))
	val missAddr = Vec.fill(4) (Reg(outType = UInt(width = 64)))
	val missCounter = new MultiCounter(4)
	val rand = LFSR16()

	// Generating stall signal
	when (missCounter.value === UInt(4) {
		io.ldStall := Bool(true)
	} .otherwise {
		io,ldStall := Bool(false)
	}

	when (io.stReq(0).addr.valid || io.stReq(1).addr.valid) {
		for (i <- 0 until 4) {
			when 	(UInt(i) < missCounter.value &&  
						(io.stReq(0).addr.bits === missAddr(i) ||
						 io.stReq(1).addr.bits === missAddr(i)) {
				io.stall := Bool(true)
			}
		}
	} .otherwise {
		io.stall := Bool(false)
	}

	// Assigning st to mem
``for (i <- 0 until 2) {
		when(!missStall) {
	    io.memStAddrPort(i).valid := io.stReq(i).addr.valid
	    io.memStAddrPort(i).bits := io.stReq(i).addr.bits
	    io.memStData(i) := io.stReq(i).data
	    /*
	     * size = 0x0 -> Store low byte
	     * size = 0x1 -> Store low half-word
	     * size = 0x2 -> Store low word
	     * size = 0x3 -> Store entire double word
	     */
	    io.memStSize(i) := io.stReq(i).size
		} .otherwise {
			io.memStAddrPort(i).valid := Bool(false)
  }
	
	// Assigning ld to mem
  io.memLdAddrPort.valid := io.ldReq.addr.valid
  io.memLdAddrPort.bits := io.ldReq.addr.bits
  io.ldReq.data.bits := io.memLdData.bits(63,0)
  io.ldReq.data.valid := io.memLdData.valid
}

class NBDCacheTests(c: NBDCache) extends Tester(c) {
  println("TODO")
}

class NBDCacheGenerator extends TestGenerator {
  def genMod(): Module = Module(new NBDCache())
  def genTest[T <: Module](c: T): Tester[T] =
    (new NBDCacheTests(c.asInstanceOf[NBDCache])).asInstanceOf[Tester[T]]
}
