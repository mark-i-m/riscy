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

class NBDCacheLdWB extends Bundle {
  val addr = Valid(UInt(OUTPUT, 64)).asOutput
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
		val ldWB = new NBDCacheLdWB
		val ldWBStall = Bool(OUTPUT)
  }
	
	var memory = Module(new BigMemory(64, 1 << 10, 5, 2, 2))
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
	val s0_ready :: s0_request :: s0_refill_init :: s0_refill_wait :: s0_refill_done :: Nil = Enum(UInt(), 5)
  val state_0 = Reg(init=s0_ready)

	val s1_ready :: s1_request :: s1_refill_init :: s1_refill_wait :: s1_refill_done :: Nil = Enum(UInt(), 5)
  val state_1 = Reg(init=s1_ready)

	val s2_ready :: s2_request :: s2_refill_init :: s2_refill_wait :: s2_refill_done :: Nil = Enum(UInt(), 5)
  val state_2 = Reg(init=s2_ready)

	val s3_ready :: s3_request :: s3_refill_init :: s3_refill_wait :: s3_refill_done :: Nil = Enum(UInt(), 5)
  val state_3 = Reg(init=s3_ready)

	val incrMiss = Bool()

	val s0_miss = Bool()
	val s1_miss = Bool()
	val s2_miss = Bool()
	val s3_miss = Bool()

  when (!io.stall && io.ldReq.addr.valid && rand >= UInt(16384)) {
		incrMiss := Bool(true)
		switch (missCounter.value) {
      is (Bits(0)) {
      	missAddr(0) := io.ldReq.addr.bits
				s0_miss			:= Bool(true)
      }
      is (Bits(1)) {
        missAddr(1) := io.ldReq.addr.bits
				s1_miss			:= Bool(true)
      }
      is (Bits(2)) {
        missAddr(2) := io.ldReq.addr.bits
				s2_miss			:= Bool(true)
      }
			is (Bits(3)) {
        missAddr(3) := io.ldReq.addr.bits
				s3_miss			:= Bool(true)
      }
		}
		memory.io.readPorts(0).valid := Bool(false)
		memory.io.readPorts(0).bits := io.ldReq.addr.bits
		io.ldReq.data.bits := memory.io.readData(0).bits(63,0)
  	io.ldReq.data.valid := Bool(false)
	} .elsewhen (io.stall) {
		memory.io.readPorts(0).valid := Bool(false)
		memory.io.readPorts(0).bits := io.ldReq.addr.bits
		io.ldReq.data.bits := memory.io.readData(0).bits(63,0)
  	io.ldReq.data.valid := Bool(false)
	} .otherwise {
		incrMiss := Bool(false)
		s0_miss := Bool(false)
		s1_miss := Bool(false)
		s2_miss := Bool(false)
		s3_miss := Bool(false)
		memory.io.readPorts(0).valid := io.ldReq.addr.valid
		memory.io.readPorts(0).bits := io.ldReq.addr.bits
		io.ldReq.data.bits := memory.io.readData(0).bits(63,0)
  	io.ldReq.data.valid := memory.io.readData(0).valid
	}

	//reg for load wb of misses 
	
	val ldWBdata = Reg(outType = Valid(UInt(width = 64)))
	val ldWBaddr = Reg(outType = Valid(UInt(width = 64)))
	val decrMiss = Bool()

	// State machine for s0
	switch (state_0) {
    is (s0_ready) {
      when (s0_miss) { state_0 := s0_refill_init }
    }
    is (s0_request) {
    }
    is (s0_refill_init) {
      state_0 := s0_refill_wait
    }
    is (s0_refill_wait) {
      when (io.ldReq.data.valid) { state_0 := s0_refill_done }
    }
    is (s0_refill_done) {
      state_0 := s0_ready
    }
  }
	
	when (state_0 === s0_refill_init) {
		memory.io.readPorts(1).valid := io.ldReq.addr.valid
		memory.io.readPorts(1).bits := io.ldReq.addr.bits
	} .otherwise {
		memory.io.readPorts(1).valid := Bool(false)
		memory.io.readPorts(1).bits := io.ldReq.addr.bits
	}
	
	when (state_0 === s0_refill_done) {
		ldWBdata.bits := memory.io.readData(1).bits(63,0)
  	ldWBdata.valid := memory.io.readData(1).valid
		ldWBaddr.bits := missAddr(0)
		ldWBaddr.valid := Bool(true)
		decrMiss := Bool(true)
		io.ldWBStall := Bool(true)
	}	.otherwise {
		ldWBdata.bits := memory.io.readData(1).bits(63,0)
  	ldWBdata.valid := memory.io.readData(1).valid
		ldWBaddr.bits := missAddr(0)
		ldWBaddr.valid := Bool(false)
		decrMiss := Bool(false)
		io.ldWBStall := Bool(false)
	}

	// State machine for s1
	switch (state_1) {
    is (s1_ready) {
      when (s1_miss) { state_1 := s1_refill_init }
    }
    is (s1_request) {
    }
    is (s1_refill_init) {
      state_1 := s1_refill_wait
    }
    is (s1_refill_wait) {
      when (io.ldReq.data.valid) { state_1 := s1_refill_done }
    }
    is (s1_refill_done) {
      state_1 := s1_ready
    }
  }
	
	when (state_1 === s1_refill_init) {
		memory.io.readPorts(2).valid := io.ldReq.addr.valid
		memory.io.readPorts(2).bits := io.ldReq.addr.bits
	} .otherwise {
		memory.io.readPorts(2).valid := Bool(false)
		memory.io.readPorts(2).bits := io.ldReq.addr.bits
	}
	
	when (state_1 === s1_refill_done) {
		ldWBdata.bits := memory.io.readData(2).bits(63,0)
  	ldWBdata.valid := memory.io.readData(2).valid
		ldWBaddr.bits := missAddr(1)
		ldWBaddr.valid := Bool(true)
		decrMiss := Bool(true)
		io.ldWBStall := Bool(true)
	} .otherwise {
		ldWBdata.bits := memory.io.readData(2).bits(63,0)
  	ldWBdata.valid := memory.io.readData(2).valid
		ldWBaddr.bits := missAddr(1)
		ldWBaddr.valid := Bool(false)
		decrMiss := Bool(false)
		io.ldWBStall := Bool(false)
	}

	// State machine for s2
	switch (state_2) {
    is (s2_ready) {
      when (s2_miss) { state_2 := s2_refill_init }
    }
    is (s2_request) {
    }
    is (s2_refill_init) {
      state_2 := s2_refill_wait
    }
    is (s2_refill_wait) {
      when (io.ldReq.data.valid) { state_2 := s2_refill_done }
    }
    is (s2_refill_done) {
      state_2 := s2_ready
    }
  }
	
	when (state_2 === s2_refill_init) {
		memory.io.readPorts(3).valid := io.ldReq.addr.valid
		memory.io.readPorts(3).bits := io.ldReq.addr.bits
	} .otherwise {
		memory.io.readPorts(3).valid := Bool(false)
		memory.io.readPorts(3).bits := io.ldReq.addr.bits
	}
	
	when (state_2 === s2_refill_done) {
		ldWBdata.bits := memory.io.readData(3).bits(63,0)
  	ldWBdata.valid := memory.io.readData(3).valid
		ldWBaddr.bits := missAddr(1)
		ldWBaddr.valid := Bool(true)
		decrMiss := Bool(true)
		io.ldWBStall := Bool(true)
	} .otherwise {
		ldWBdata.bits := memory.io.readData(3).bits(63,0)
  	ldWBdata.valid := memory.io.readData(3).valid
		ldWBaddr.bits := missAddr(1)
		ldWBaddr.valid := Bool(false)
		decrMiss := Bool(false)
		io.ldWBStall := Bool(false)
	}

	// State machine for s2
	switch (state_3) {
    is (s3_ready) {
      when (s3_miss) { state_3 := s3_refill_init }
    }
    is (s3_request) {
    }
    is (s3_refill_init) {
      state_3 := s3_refill_wait
    }
    is (s3_refill_wait) {
      when (io.ldReq.data.valid) { state_3 := s3_refill_done }
    }
    is (s3_refill_done) {
      state_3 := s3_ready
    }
  }
	
	when (state_3 === s3_refill_init) {
		memory.io.readPorts(4).valid := io.ldReq.addr.valid
		memory.io.readPorts(4).bits := io.ldReq.addr.bits
	} .otherwise {
		memory.io.readPorts(4).valid := Bool(false)
		memory.io.readPorts(4).bits := io.ldReq.addr.bits
	}
	
	when (state_3 === s3_refill_done) {
		ldWBdata.bits := memory.io.readData(4).bits(63,0)
  	ldWBdata.valid := memory.io.readData(4).valid
		ldWBaddr.bits := missAddr(1)
		ldWBaddr.valid := Bool(true)
		decrMiss := Bool(true)
		io.ldWBStall := Bool(true)
	} .otherwise {
		ldWBdata.bits := memory.io.readData(4).bits(63,0)
  	ldWBdata.valid := memory.io.readData(4).valid
		ldWBaddr.bits := missAddr(1)
		ldWBaddr.valid := Bool(false)
		decrMiss := Bool(false)
		io.ldWBStall := Bool(false)
	}

	io.ldWB.addr := ldWBaddr
	io.ldWB.data := ldWBdata

	//calculating counter value for miss table
	when (incrMiss && !decrMiss) {
  	missCounter.inc(1)
 	} .elsewhen (!incrMiss && decrMiss) {
   	missCounter.dec(1)
	}
	
	//TODO: Implement shift logic
	//TODO: decide on which place to compare store with
	//TODO: Decide on number of wait states
}

class NBDCacheTests(c: NBDCache) extends Tester(c) {
  println("TODO")
}

class NBDCacheGenerator extends TestGenerator {
  def genMod(): Module = Module(new NBDCache())
  def genTest[T <: Module](c: T): Tester[T] =
    (new NBDCacheTests(c.asInstanceOf[NBDCache])).asInstanceOf[Tester[T]]
}
