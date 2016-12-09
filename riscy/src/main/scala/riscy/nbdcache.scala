package riscy

import Chisel._
import scala.language.reflectiveCalls

class NBDCacheStReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = UInt(INPUT, 64)
  val size = UInt(INPUT, 3)
}

class NBDCacheLdReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = Valid(UInt(OUTPUT, 64)).asOutput
}

class NBDCache extends Module {
  val io = new Bundle {
    val stReq = Vec(2, new NBDCacheStReq)
    val ldReq = new NBDCacheLdReq
    // In-module memory - so do not need it
		//val memStAddrPort = Vec(2, Valid(UInt(OUTPUT,64).asOutput))
    //val memStData = Vec(2, UInt(OUTPUT,64))
    //val memStSize = Vec(2, UInt(OUTPUT,3))
    //val memLdAddrPort = Valid(UInt(OUTPUT,64)).asOutput
    //val memLdData = Valid(UInt(INPUT,8 * 64)).asInput
		val ldStall = Bool(OUTPUT)
		val stall = Bool(OUTPUT)

  }
	
	var memory = Module(new BigMemory(64, 1 << 10, 2, 2, 2))
	val missCount = Reg(init=UInt(0, width = 3))
	val missAddr = Vec.fill(4) (Reg(outType = UInt(width = 64)))
	val missCounter = new MultiCounter(4)
	val rand = LFSR16()

	// Generating stall signal for loads
	// If already four load misses stall
	when (missCounter.value === UInt(4)) {
		io.ldStall := Bool(true)
	} .otherwise {
		io.ldStall := Bool(false)
	}

	when (io.stReq(0).addr.valid) {
		for (i <- 0 until 4) {
			when 	(UInt(i) < missCounter.value &&  
						io.stReq(0).addr.bits === missAddr(i)) {
				io.stall := Bool(true)
			}
		}
	} .elsewhen (io.stReq(1).addr.valid) {
		for (i <- 0 until 4) {
			when 	(UInt(i) < missCounter.value &&  
						io.stReq(1).addr.bits === missAddr(i)) {
				io.stall := Bool(true)
			}
		}
	} .otherwise {
		io.stall := Bool(false)
	}

	// Assigning st to mem
	for (i <- 0 until 2) {
		when (!io.stall) {
	    memory.io.writePorts(i).valid := io.stReq(i).addr.valid
	    memory.io.writePorts(i).bits := io.stReq(i).addr.bits
	    memory.io.writeData(i) := io.stReq(i).data
	    /*
	     * size = 0x0 -> Store low byte
	     * size = 0x1 -> Store low half-word
	     * size = 0x2 -> Store low word
	     * size = 0x3 -> Store entire double word
	     */
	    memory.io.writeSize(i) := io.stReq(i).size
		} .otherwise {
			memory.io.writePorts(i).valid := Bool(false)
			memory.io.writePorts(i).bits := io.stReq(i).addr.bits
	    memory.io.writeData(i) := io.stReq(i).data
	    /*
	     * size = 0x0 -> Store low byte
	     * size = 0x1 -> Store low half-word
	     * size = 0x2 -> Store low word
	     * size = 0x3 -> Store entire double word
	     */
	    memory.io.writeSize(i) := io.stReq(i).size
  	}
	}
	
	// Assigning ld to mem
	
	// miss state definition
	val s0_ready :: s0_miss :: s0_mem_init :: s0_mem_wait :: s0_mem_done :: Nil = Enum(UInt(), 5)
  val state_0 = Reg(init=s0_ready)

	val s1_ready :: s1_miss :: s1_mem_init :: s1_mem_wait :: s1_mem_done :: Nil = Enum(UInt(), 5)
  val state_1 = Reg(init=s1_ready)

	val s2_ready :: s2_miss :: s2_mem_init :: s2_mem_wait :: s2_mem_done :: Nil = Enum(UInt(), 5)
  val state_2 = Reg(init=s2_ready)

	val s3_ready :: s3_miss :: s3_mem_init :: s3_mem_wait :: s3_mem_done :: Nil = Enum(UInt(), 5)
  val state_3 = Reg(init=s3_ready)

	val incrMiss = Bool(false)

  when (!io.stall && io.ldReq.addr.valid && rand >= UInt(16384)) {
		incrMiss := Bool(true)
		switch (missCounter.value) {
      is (Bits(0)) {
      	missAddr(0) := io.ldReq.addr.bits
				state_0 := s0_miss
      }
      is (Bits(1)) {
        missAddr(1) := io.ldReq.addr.bits
				state_1 := s1_miss
      }
      is (Bits(2)) {
        missAddr(2) := io.ldReq.addr.bits
				state_2 := s2_miss
      }
			is (Bits(3)) {
        missAddr(3) := io.ldReq.addr.bits
				state_3 := s3_miss
      }
		}
		memory.io.readPorts(0).valid := Bool(false)
		memory.io.readPorts(0).bits := io.ldReq.addr.bits
  	io.ldReq.data.bits := memory.io.readData(0).bits(63,0)
  	io.ldReq.data.valid := Bool(false)
	} .elsewhen (io.stall) {
	} .otherwise {
		incrMiss := Bool(false)
		memory.io.readPorts(0).valid := io.ldReq.addr.valid
		memory.io.readPorts(0).bits := io.ldReq.addr.bits
		io.ldReq.data.bits := memory.io.readData(0).bits(63,0)
  	io.ldReq.data.valid := memory.io.readData(0).valid
	}	
}

class NBDCacheTests(c: NBDCache) extends Tester(c) {
  println("TODO")
}

class NBDCacheGenerator extends TestGenerator {
  def genMod(): Module = Module(new NBDCache())
  def genTest[T <: Module](c: T): Tester[T] =
    (new NBDCacheTests(c.asInstanceOf[NBDCache])).asInstanceOf[Tester[T]]
}
