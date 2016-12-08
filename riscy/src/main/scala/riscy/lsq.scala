package riscy

import Chisel._
import scala.language.reflectiveCalls

class LSQEntry extends AddrBufEntry {
  val addr = Valid(UInt(OUTPUT, 32))
  val value = Valid(UInt(OUTPUT, 64))
  val ready  = Bool(OUTPUT) // Entry populated or not
}

class LSQ extends Module {
  val io = new Bundle {
    val resEntry = Vec.fill(4) { Valid(new AddrBufEntry).flip }
    val robWbin = new RobWbStore(6).flip
    val stCommit = Vec(2, Valid(UInt(INPUT, 6)).asInput)
    val currentLen = UInt(OUTPUT, 4)
    val robWbOut = new RobWbInput(2).flip
    val ldAddr = Valid(UInt(OUTPUT, 64))
    val ldValue = UInt(INPUT, 64)
    val memStAddrPort = Vec(2, Valid(UInt(OUTPUT,64).asOutput))
    val memStData = Vec(2, UInt(OUTPUT,64))
    val memStSize = Vec(2, UInt(OUTPUT,3))
    val memLdAddrPort = Valid(UInt(OUTPUT,64)).asOutput
    val memLdData = Valid(UInt(INPUT,8 * 64)).asInput
  }

  // The Data Cache
  val dcache = Module(new DCache())

  // Hook up D$ to memory
  io.memStAddrPort := dcache.io.memStAddrPort
  io.memStData := dcache.io.memStData
  io.memStSize := dcache.io.memStSize
  io.memLdAddrPort := dcache.io.memLdAddrPort
  dcache.io.memLdData := io.memLdData

  val depMatrix = Vec.tabulate(32) { i => Reg(Valid(Vec.fill(32) { Bool() } )) }
  val depRow = Vec.tabulate(32) { i => Cat(Array.tabulate(32) { depMatrix(i).bits(_) }) }

  val addrqW = Vec.fill(32) { Valid(new LSQEntry) }
  val addrq = Vec.tabulate(32) { i => Reg(addrqW(i)) }

  val pos = Vec(4, UInt(width=5))

  io.currentLen := PopCount(Array.tabulate(32) { addrq(_).valid })

  val DEPTH = 32
  val WbCamAddr = Module(new CAM(8, DEPTH, 6))
  val WbCamValue = Module(new CAM(12, DEPTH, 6))

  for ( i <- 0 until 32) {
    WbCamAddr.io.input_bits(i) := addrq(i).bits.robLoc
    WbCamValue.io.input_bits(i) := addrq(i).bits.robLoc
  }

  // Addresses should be generated by ALUs and hence in the first 4 entries only
  for ( i <- 0 until 4) {
    //TODO
    WbCamAddr.io.compare_bits(i) := io.robWbin.entry_s1(i).operand
    WbCamAddr.io.compare_bits(i+4) := io.robWbin.entry_s2(i).operand
  }

  for (i <- 0 until 6) {
    WbCamValue.io.compare_bits(i) := io.robWbin.entry_s1(i).operand
    WbCamValue.io.compare_bits(i+6) := io.robWbin.entry_s2(i).operand
  }

  // The Allocation logic
  val addrqAlloc = Module(new AddrQueueAlloc)
  addrqAlloc.io.validEntries := Array.tabulate(32) { addrq(_).valid }
  pos := addrqAlloc.io.pos

  for (i <- 0 until 32) {
    when (UInt(i) === pos(0)) {
    addrq(i) := io.resEntry(0)
    }
    when (UInt(i) === pos(1)) {
    addrq(i) := io.resEntry(1)
    }
    when (UInt(i) === pos(2)) {
    addrq(i) := io.resEntry(2)
    }
    when (UInt(i) === pos(3)) {
    addrq(i) := io.resEntry(3)
    }
  }

  for ( i <- 0 until DEPTH) {
    for ( j <- 0 until 4) {
      when (WbCamAddr.io.hit(j)(i) && io.robWbin.entry_s1(j).valid
              && io.robWbin.entry_s1(j).is_addr) {
        addrq(i).bits.addr.valid := Bool(true)
        addrq(i).bits.addr.bits := io.robWbin.entry_s1(j).data
      } .elsewhen (WbCamAddr.io.hit(j+4)(i) && io.robWbin.entry_s2(j).valid
                    && io.robWbin.entry_s2(j).is_addr) {
        addrq(i).bits.addr.valid := Bool(true)
        addrq(i).bits.addr.bits := io.robWbin.entry_s2(j).data
      }
    }
    for ( j <- 0 until 6) {
      when (WbCamValue.io.hit(j)(i) && io.robWbin.entry_s1(j).valid
              && !io.robWbin.entry_s1(j).is_addr && addrq(i).bits.st_nld) {
        addrq(i).bits.value.valid := Bool(true)
        addrq(i).bits.value.bits := io.robWbin.entry_s1(j).data
      } .elsewhen (WbCamValue.io.hit(j+6)(i) && io.robWbin.entry_s2(j).valid
                    && !io.robWbin.entry_s2(j).is_addr && addrq(i).bits.st_nld) {
        addrq(i).bits.value.valid := Bool(true)
        addrq(i).bits.value.bits := io.robWbin.entry_s2(j).data
      }
    }
  }

  val CamAddrMatch = Module(new CAM(DEPTH, DEPTH, 65))
  for (i <- 0 until DEPTH) {
    CamAddrMatch.io.compare_bits(i) := addrq(i).bits.addr.bits
    CamAddrMatch.io.input_bits(i) := addrq(i).bits.addr.bits
  }

  for (i <- 0 until DEPTH) {
    for (j <- 0 until DEPTH) {
      if (i != j) {
        when (addrq(i).valid && addrq(i).bits.addr.valid 
              && addrq(j).bits.addr.valid && CamAddrMatch.io.hit(j)(i)
              && (addrq(i).bits.robLoc < addrq(j).bits.robLoc)) {
          depMatrix(i).bits(j) := Bool(true)
        }
      }
    }
  }

  val loads = Vec.tabulate(32) { i => (addrq(i).valid
              && !addrq(i).bits.st_nld && !addrq(i).bits.value.valid
              && addrq(i).bits.addr.valid)}

  val isLoad = Cat(Array.tabulate(32) {loads(_)})
  when (isLoad.orR) {
    dcache.io.ldReq.addr.valid := Bool(true)
    dcache.io.ldReq.addr.bits := addrq(PriorityEncoder(loads)).bits.addr.bits
    when (dcache.io.ldReq.data.valid && !addrq(PriorityEncoder(loads)).bits.value.valid) {
      switch (addrq(PriorityEncoder(loads)).bits.funct3) {
        is (UInt(0x3)) {
          addrq(PriorityEncoder(loads)).bits.value.bits := dcache.io.ldReq.data.bits
        }
        is (UInt(0x2)) {
          addrq(PriorityEncoder(loads)).bits.value.bits :=
            Cat(Fill(32,dcache.io.ldReq.data.bits(31)),
              dcache.io.ldReq.data.bits(31,0))
        }
        is (UInt(0x6)) {
          addrq(PriorityEncoder(loads)).bits.value.bits :=
            Cat(Fill(32,UInt(0,width=1)),
              dcache.io.ldReq.data.bits(31,0))
        }
        is (UInt(0x1)) {
          addrq(PriorityEncoder(loads)).bits.value.bits :=
            Cat(Fill(48,dcache.io.ldReq.data.bits(15)),
              dcache.io.ldReq.data.bits(15,0))
        }
        is (UInt(0x5)) {
          addrq(PriorityEncoder(loads)).bits.value.bits :=
            Cat(Fill(48,UInt(0,width=1)),
              dcache.io.ldReq.data.bits(15,0))
        }
        is (UInt(0x0)) {
          addrq(PriorityEncoder(loads)).bits.value.bits :=
            Cat(Fill(56,dcache.io.ldReq.data.bits(7)),
              dcache.io.ldReq.data.bits(7,0))
        }
        is (UInt(0x4)) {
          addrq(PriorityEncoder(loads)).bits.value.bits :=
            Cat(Fill(56,UInt(0,width=1)),
              dcache.io.ldReq.data.bits(7,0))
        }
      }
      addrq(PriorityEncoder(loads)).bits.value.valid := Bool(true)
    }
  } .otherwise {
    dcache.io.ldReq.addr.valid := Bool(false)
    dcache.io.ldReq.addr.bits := UInt(0xdead)
  }

  val CamStCommit = Module(new CAM(2, DEPTH, 6))
  val stCommitRow = Vec(2, Vec(DEPTH, Bool()))
  val stCommitSet = Vec(2, Bool())
  val ldIssueRow = Vec(DEPTH, Bool())
  // Load selection logic
  val ldSelect = Module(new AddrQueueAlloc)
  ldSelect.io.validEntries := Array.tabulate(32) { !ldIssueRow(_) }

  for ( i <- 0 until 32) {
    CamStCommit.io.input_bits(i) := addrq(i).bits.robLoc
  }

  for ( i <- 0 until 2) {
    CamStCommit.io.compare_bits(i) := io.stCommit(i).bits
  }

  val dispatch = Vec.fill(32) { Bool() }
  for (i <- 0 until DEPTH) {
    when (addrq(i).valid
          && addrq(i).bits.addr.valid && addrq(i).bits.value.valid) {
      addrq(i).bits.ready := Bool(true)
      when (!depRow(i).orR) {
        dispatch(i) := Bool(true)
      }
    } .otherwise {
      addrq(i).bits.ready := Bool(false)
      dispatch(i) := Bool(false)
    }

    for (j <- 0 until 2) {
      when(dispatch(i)) {
        printf("Ready to dispatch %d\n", UInt(i))
          for(k <- 0 until DEPTH) {
            depMatrix(k).bits(i) := Bool(false)
          }
          when (!addrq(i).bits.st_nld) {
            ldIssueRow(i) := Bool(true)
            addrq(i).bits.addr.valid := Bool(false)
            addrq(i).bits.value.valid := Bool(false)
            addrq(i).valid := Bool(false)
          } .elsewhen (CamStCommit.io.hit(j)(i)) {
            printf("St dispatch on %d\n", UInt(j))
            printf("St dispatch addr %d\n", addrq(i).bits.addr.bits)
            printf("St dispatch value %d\n", addrq(i).bits.value.bits)
            addrq(i).bits.addr.valid := Bool(false)
            addrq(i).valid := Bool(false)
            stCommitRow(j)(i) := Bool(true)
          }
      } .otherwise {
        stCommitRow(j)(i) := Bool(false)
        ldIssueRow(i) := Bool(false)
      }
    }
  }

  for (i <- 0 until 2) {
    stCommitSet(i) := Cat(Array.tabulate(32) { stCommitRow(i)(_) }).orR
    when (stCommitSet(i)) {
      dcache.io.stReq(i).addr.valid := Bool(true)
      dcache.io.stReq(i).addr.bits := addrq(PriorityEncoder(stCommitRow(i))).bits.addr.bits
      dcache.io.stReq(i).data := addrq(PriorityEncoder(stCommitRow(i))).bits.value.bits
      dcache.io.stReq(i).size := addrq(PriorityEncoder(stCommitRow(i))).bits.funct3
    } .otherwise {
      dcache.io.stReq(i).addr.valid := Bool(false)
      dcache.io.stReq(i).addr.bits := UInt(0xdead)
      dcache.io.stReq(i).data := UInt(0xdead)
      dcache.io.stReq(i).size := UInt(0x3)
    }

    val ldIssueSet = Cat(Array.tabulate(32) { ldIssueRow(_) }).orR

    when(ldIssueSet) {
      io.robWbOut.entry(i).data := addrq(ldSelect.io.pos(i)).bits.value.bits
      io.robWbOut.entry(i).is_addr := Bool(false)
      io.robWbOut.entry(i).operand := addrq(ldSelect.io.pos(i)).bits.robLoc
      io.robWbOut.entry(i).valid := Bool(true)
    } .otherwise {
      io.robWbOut.entry(i).data := UInt(0xdead)
      io.robWbOut.entry(i).is_addr := Bool(false)
      io.robWbOut.entry(i).operand := UInt(0x0)
      io.robWbOut.entry(i).valid := Bool(false)
    }
  }
}

class LSQTests(c: LSQ) extends Tester(c) {
  // Queue should be empty initially
  expect(c.io.currentLen, 0)
  expect(c.addrq(0).bits.st_nld, 0)
  expect(c.depMatrix(1).bits(0), false)

  // Reserve one entry from the arbiter
  poke(c.io.resEntry(0).bits.st_nld, 1)
  poke(c.io.resEntry(0).bits.robLoc, 6)
  poke(c.io.resEntry(0).valid, 1)

  step(1)
  // Cycle 1

  // First entry should be reserved
  expect(c.addrq(0).bits.st_nld, 1)
  expect(c.addrq(0).valid, 1)

  // Second entry should not be reserved
  expect(c.addrq(1).bits.st_nld, 0)
  expect(c.addrq(1).valid, 0)

  // Queue has one entry
  expect(c.io.currentLen, 1)

  // Reserve two entries from the arbiter
  poke(c.io.resEntry(0).bits.robLoc, 4)
  poke(c.io.resEntry(1).bits.st_nld, 0)
  poke(c.io.resEntry(1).bits.robLoc, 8)
  poke(c.io.resEntry(1).valid, 1)
  expect(c.WbCamAddr.io.hit(2)(0), false)

  step(1)
  // Cycle 2

  // Verify entries have been correctly stored
  expect(c.addrq(1).bits.st_nld, 1)
  expect(c.addrq(1).valid, 1)
  expect(c.addrq(2).bits.st_nld, 0)
  expect(c.addrq(2).valid, 1)

  expect(c.io.currentLen, 3)

  // Reserve four entries
  poke(c.io.resEntry(2).bits.st_nld, 0)
  poke(c.io.resEntry(3).bits.st_nld, 1)
  poke(c.io.resEntry(2).valid, 1)
  poke(c.io.resEntry(3).valid, 1)
  poke(c.io.resEntry(0).bits.robLoc, 2)
  poke(c.io.resEntry(1).bits.robLoc, 5)
  poke(c.io.resEntry(2).bits.robLoc, 9)
  poke(c.io.resEntry(3).bits.robLoc, 15)

  poke(c.io.robWbin.entry_s1(2).operand, 6)
  poke(c.io.robWbin.entry_s1(2).is_addr, true)
  poke(c.io.robWbin.entry_s1(2).valid, true)
  poke(c.io.robWbin.entry_s1(2).data, 0x10000)
  expect(c.WbCamAddr.io.hit(2)(0), true)
  //expect(c.addrq(0).bits.addr.valid, false)
  expect(c.WbCamAddr.io.hit(7)(0), false)
  expect(c.depMatrix(1).bits(0), false)

  step(1)
  // Cycle 3

  expect(c.io.currentLen, 7)

  expect(c.addrq(3).bits.st_nld, 1)
  expect(c.addrq(3).bits.robLoc, 2)
  expect(c.addrq(3).valid, 1)
  expect(c.addrq(4).bits.st_nld, 0)
  expect(c.addrq(4).bits.robLoc, 5)
  expect(c.addrq(4).valid, 1)
  expect(c.addrq(5).bits.st_nld, 0)
  expect(c.addrq(5).bits.robLoc, 9)
  expect(c.addrq(5).valid, 1)
  expect(c.addrq(6).bits.st_nld, 1)
  expect(c.addrq(6).bits.robLoc, 15)
  expect(c.addrq(6).valid, 1)

  //expect(c.addrq(0).bits.addr.valid, true)
  expect(c.addrq(0).bits.addr.bits, 0x10000)

  //
  expect(c.WbCamAddr.io.hit(2)(0), true)
  poke(c.io.robWbin.entry_s2(3).operand, 6)
  poke(c.io.robWbin.entry_s2(3).is_addr, false)
  poke(c.io.robWbin.entry_s2(3).valid, true)
  poke(c.io.robWbin.entry_s2(3).data, 0xff)
  expect(c.WbCamValue.io.hit(9)(0), true)
  //expect(c.addrq(0).bits.value.valid, false)
  poke(c.io.robWbin.entry_s1(1).operand, 4)
  poke(c.io.robWbin.entry_s1(1).is_addr, true)
  poke(c.io.robWbin.entry_s1(1).valid, true)
  poke(c.io.robWbin.entry_s1(1).data, 0x10000)
  expect(c.depMatrix(1).bits(0), false)

  // Reserve no more entries
  poke(c.io.resEntry(0).valid, 0)
  poke(c.io.resEntry(1).valid, 0)
  poke(c.io.resEntry(2).valid, 0)
  poke(c.io.resEntry(3).valid, 0)

  step(1)
  expect(c.addrq(0).bits.value.bits, 0xff)
  expect(c.addrq(1).bits.addr.bits, 0x10000)
  expect(c.addrq(0).bits.value.valid, true)
  step(1)
  // Cycle 4

  //expect(c.addrq(0).bits.value.valid, true)
  //expect(c.addrq(0).bits.ready, true)
  expect(c.depMatrix(1).bits(0), false)

  poke(c.io.stCommit(0).valid, true)
  poke(c.io.stCommit(0).bits, 6)
  expect(c.CamStCommit.io.hit(0)(0), true)
  expect(c.dcache.io.stReq(0).addr.valid, true)
  expect(c.dcache.io.stReq(0).addr.bits, 0x10000)

  step(1)
  expect(c.depMatrix(1).bits(0), false)
}

class LSQGenerator extends TestGenerator {
  def genMod(): Module = Module(new LSQ())
  def genTest[T <: Module](c: T): Tester[T] =
    (new LSQTests(c.asInstanceOf[LSQ])).asInstanceOf[Tester[T]]
}
