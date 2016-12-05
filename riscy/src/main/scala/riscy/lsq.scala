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
    val stAddr = Vec(2, Valid(UInt(OUTPUT, 64)))
    val stValue = Vec(2, UInt(OUTPUT, 64))
    val memStAddrPort = Vec(2, Valid(UInt(OUTPUT,64).asOutput))
    val memStData = Vec(2, UInt(OUTPUT,64))
    val memLdAddrPort = Valid(UInt(OUTPUT,64)).asOutput
    val memLdData = Valid(UInt(INPUT,8 * 64)).asInput
  }

  // The Data Cache
  val dcache = Module(new DCache())

  // Hook up D$ to memory
  io.memStAddrPort := dcache.io.memStAddrPort
  io.memStData := dcache.io.memStData
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
    WbCamAddr.io.compare_bits(i) := io.robWbin.operand_s1(i)
    WbCamAddr.io.compare_bits(i+4) := io.robWbin.operand_s2(i)
  }

  for (i <- 0 until 6) {
    WbCamValue.io.compare_bits(i) := io.robWbin.operand_s1(i)
    WbCamValue.io.compare_bits(i+6) := io.robWbin.operand_s2(i)
  }

  // Allocation logic
  // Should clean up and move this to another module
  pos(0) := PriorityEncoder(Vec.tabulate(32) { i => !addrq(i).valid })
  when(pos(0) === UInt(1)) {
    pos(1) := UInt(2) + PriorityEncoder(Vec.tabulate(32-1-1) { i => !addrq(i+1+1).valid })
  } .elsewhen (pos(0) === UInt(2)) {
    pos(1) := UInt(3) + PriorityEncoder(Vec.tabulate(32-2-1) { i => !addrq(i+2+1).valid })
  } .elsewhen (pos(0) === UInt(3)) {
    pos(1) := UInt(4) + PriorityEncoder(Vec.tabulate(32-3-1) { i => !addrq(i+3+1).valid })
  } .elsewhen (pos(0) === UInt(4)) {
    pos(1) := UInt(5) + PriorityEncoder(Vec.tabulate(32-4-1) { i => !addrq(i+4+1).valid })
  } .elsewhen (pos(0) === UInt(5)) {
    pos(1) := UInt(6) + PriorityEncoder(Vec.tabulate(32-5-1) { i => !addrq(i+5+1).valid })
  } .elsewhen (pos(0) === UInt(6)) {
    pos(1) := UInt(7) + PriorityEncoder(Vec.tabulate(32-6-1) { i => !addrq(i+6+1).valid })
  } .elsewhen (pos(0) === UInt(7)) {
    pos(1) := UInt(8) + PriorityEncoder(Vec.tabulate(32-7-1) { i => !addrq(i+7+1).valid })
  } .elsewhen (pos(0) === UInt(8)) {
    pos(1) := UInt(9) + PriorityEncoder(Vec.tabulate(32-8-1) { i => !addrq(i+8+1).valid })
  } .elsewhen (pos(0) === UInt(9)) {
    pos(1) := UInt(10) + PriorityEncoder(Vec.tabulate(32-9-1) { i => !addrq(i+9+1).valid })
  } .elsewhen (pos(0) === UInt(10)) {
    pos(1) := UInt(11) + PriorityEncoder(Vec.tabulate(32-10-1) { i => !addrq(i+10+1).valid })
  } .elsewhen (pos(0) === UInt(11)) {
    pos(1) := UInt(12) + PriorityEncoder(Vec.tabulate(32-11-1) { i => !addrq(i+11+1).valid })
  } .elsewhen (pos(0) === UInt(12)) {
    pos(1) := UInt(13) + PriorityEncoder(Vec.tabulate(32-12-1) { i => !addrq(i+12+1).valid })
  } .elsewhen (pos(0) === UInt(13)) {
    pos(1) := UInt(14) + PriorityEncoder(Vec.tabulate(32-13-1) { i => !addrq(i+13+1).valid })
  } .elsewhen (pos(0) === UInt(14)) {
    pos(1) := UInt(15) + PriorityEncoder(Vec.tabulate(32-14-1) { i => !addrq(i+14+1).valid })
  } .elsewhen (pos(0) === UInt(15)) {
    pos(1) := UInt(16) + PriorityEncoder(Vec.tabulate(32-15-1) { i => !addrq(i+15+1).valid })
  } .elsewhen (pos(0) === UInt(16)) {
    pos(1) := UInt(17) + PriorityEncoder(Vec.tabulate(32-16-1) { i => !addrq(i+16+1).valid })
  } .elsewhen (pos(0) === UInt(17)) {
    pos(1) := UInt(18) + PriorityEncoder(Vec.tabulate(32-17-1) { i => !addrq(i+17+1).valid })
  } .elsewhen (pos(0) === UInt(18)) {
    pos(1) := UInt(19) + PriorityEncoder(Vec.tabulate(32-18-1) { i => !addrq(i+18+1).valid })
  } .elsewhen (pos(0) === UInt(19)) {
    pos(1) := UInt(20) + PriorityEncoder(Vec.tabulate(32-19-1) { i => !addrq(i+19+1).valid })
  } .elsewhen (pos(0) === UInt(20)) {
    pos(1) := UInt(21) + PriorityEncoder(Vec.tabulate(32-20-1) { i => !addrq(i+20+1).valid })
  } .elsewhen (pos(0) === UInt(21)) {
    pos(1) := UInt(22) + PriorityEncoder(Vec.tabulate(32-21-1) { i => !addrq(i+21+1).valid })
  } .elsewhen (pos(0) === UInt(22)) {
    pos(1) := UInt(23) + PriorityEncoder(Vec.tabulate(32-22-1) { i => !addrq(i+22+1).valid })
  } .elsewhen (pos(0) === UInt(23)) {
    pos(1) := UInt(24) + PriorityEncoder(Vec.tabulate(32-23-1) { i => !addrq(i+23+1).valid })
  } .elsewhen (pos(0) === UInt(24)) {
    pos(1) := UInt(25) + PriorityEncoder(Vec.tabulate(32-24-1) { i => !addrq(i+24+1).valid })
  } .elsewhen (pos(0) === UInt(25)) {
    pos(1) := UInt(26) + PriorityEncoder(Vec.tabulate(32-25-1) { i => !addrq(i+25+1).valid })
  } .elsewhen (pos(0) === UInt(26)) {
    pos(1) := UInt(27) + PriorityEncoder(Vec.tabulate(32-26-1) { i => !addrq(i+26+1).valid })
  } .elsewhen (pos(0) === UInt(27)) {
    pos(1) := UInt(28) + PriorityEncoder(Vec.tabulate(32-27-1) { i => !addrq(i+27+1).valid })
  } .elsewhen (pos(0) === UInt(28)) {
    pos(1) := UInt(29) + PriorityEncoder(Vec.tabulate(32-28-1) { i => !addrq(i+28+1).valid })
  } .elsewhen (pos(0) === UInt(29)) {
    pos(1) := UInt(30) + PriorityEncoder(Vec.tabulate(32-29-1) { i => !addrq(i+29+1).valid })
  } .elsewhen (pos(0) === UInt(30)) {
    pos(1) := UInt(31) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !addrq(i+30+1).valid })
  } .elsewhen (pos(0) === UInt(31)) {
    pos(1) := UInt(3) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !addrq(i+30+1).valid })
  } .otherwise {
    pos(1) := UInt(1) + PriorityEncoder(Vec.tabulate(32-0-1) { i => !addrq(i+0+1).valid })
  }

  when(pos(1) === UInt(1)) {
    pos(2) := UInt(2) + PriorityEncoder(Vec.tabulate(32-1-1) { i => !addrq(i+1+1).valid })
  } .elsewhen (pos(1) === UInt(2)) {
    pos(2) := UInt(3) + PriorityEncoder(Vec.tabulate(32-2-1) { i => !addrq(i+2+1).valid })
  } .elsewhen (pos(1) === UInt(3)) {
    pos(2) := UInt(4) + PriorityEncoder(Vec.tabulate(32-3-1) { i => !addrq(i+3+1).valid })
  } .elsewhen (pos(1) === UInt(4)) {
    pos(2) := UInt(5) + PriorityEncoder(Vec.tabulate(32-4-1) { i => !addrq(i+4+1).valid })
  } .elsewhen (pos(1) === UInt(5)) {
    pos(2) := UInt(6) + PriorityEncoder(Vec.tabulate(32-5-1) { i => !addrq(i+5+1).valid })
  } .elsewhen (pos(1) === UInt(6)) {
    pos(2) := UInt(7) + PriorityEncoder(Vec.tabulate(32-6-1) { i => !addrq(i+6+1).valid })
  } .elsewhen (pos(1) === UInt(7)) {
    pos(2) := UInt(8) + PriorityEncoder(Vec.tabulate(32-7-1) { i => !addrq(i+7+1).valid })
  } .elsewhen (pos(1) === UInt(8)) {
    pos(2) := UInt(9) + PriorityEncoder(Vec.tabulate(32-8-1) { i => !addrq(i+8+1).valid })
  } .elsewhen (pos(1) === UInt(9)) {
    pos(2) := UInt(10) + PriorityEncoder(Vec.tabulate(32-9-1) { i => !addrq(i+9+1).valid })
  } .elsewhen (pos(1) === UInt(10)) {
    pos(2) := UInt(11) + PriorityEncoder(Vec.tabulate(32-10-1) { i => !addrq(i+10+1).valid })
  } .elsewhen (pos(1) === UInt(11)) {
    pos(2) := UInt(12) + PriorityEncoder(Vec.tabulate(32-11-1) { i => !addrq(i+11+1).valid })
  } .elsewhen (pos(1) === UInt(12)) {
    pos(2) := UInt(13) + PriorityEncoder(Vec.tabulate(32-12-1) { i => !addrq(i+12+1).valid })
  } .elsewhen (pos(1) === UInt(13)) {
    pos(2) := UInt(14) + PriorityEncoder(Vec.tabulate(32-13-1) { i => !addrq(i+13+1).valid })
  } .elsewhen (pos(1) === UInt(14)) {
    pos(2) := UInt(15) + PriorityEncoder(Vec.tabulate(32-14-1) { i => !addrq(i+14+1).valid })
  } .elsewhen (pos(1) === UInt(15)) {
    pos(2) := UInt(16) + PriorityEncoder(Vec.tabulate(32-15-1) { i => !addrq(i+15+1).valid })
  } .elsewhen (pos(1) === UInt(16)) {
    pos(2) := UInt(17) + PriorityEncoder(Vec.tabulate(32-16-1) { i => !addrq(i+16+1).valid })
  } .elsewhen (pos(1) === UInt(17)) {
    pos(2) := UInt(18) + PriorityEncoder(Vec.tabulate(32-17-1) { i => !addrq(i+17+1).valid })
  } .elsewhen (pos(1) === UInt(18)) {
    pos(2) := UInt(19) + PriorityEncoder(Vec.tabulate(32-18-1) { i => !addrq(i+18+1).valid })
  } .elsewhen (pos(1) === UInt(19)) {
    pos(2) := UInt(20) + PriorityEncoder(Vec.tabulate(32-19-1) { i => !addrq(i+19+1).valid })
  } .elsewhen (pos(1) === UInt(20)) {
    pos(2) := UInt(21) + PriorityEncoder(Vec.tabulate(32-20-1) { i => !addrq(i+20+1).valid })
  } .elsewhen (pos(1) === UInt(21)) {
    pos(2) := UInt(22) + PriorityEncoder(Vec.tabulate(32-21-1) { i => !addrq(i+21+1).valid })
  } .elsewhen (pos(1) === UInt(22)) {
    pos(2) := UInt(23) + PriorityEncoder(Vec.tabulate(32-22-1) { i => !addrq(i+22+1).valid })
  } .elsewhen (pos(1) === UInt(23)) {
    pos(2) := UInt(24) + PriorityEncoder(Vec.tabulate(32-23-1) { i => !addrq(i+23+1).valid })
  } .elsewhen (pos(1) === UInt(24)) {
    pos(2) := UInt(25) + PriorityEncoder(Vec.tabulate(32-24-1) { i => !addrq(i+24+1).valid })
  } .elsewhen (pos(1) === UInt(25)) {
    pos(2) := UInt(26) + PriorityEncoder(Vec.tabulate(32-25-1) { i => !addrq(i+25+1).valid })
  } .elsewhen (pos(1) === UInt(26)) {
    pos(2) := UInt(27) + PriorityEncoder(Vec.tabulate(32-26-1) { i => !addrq(i+26+1).valid })
  } .elsewhen (pos(1) === UInt(27)) {
    pos(2) := UInt(28) + PriorityEncoder(Vec.tabulate(32-27-1) { i => !addrq(i+27+1).valid })
  } .elsewhen (pos(1) === UInt(28)) {
    pos(2) := UInt(29) + PriorityEncoder(Vec.tabulate(32-28-1) { i => !addrq(i+28+1).valid })
  } .elsewhen (pos(1) === UInt(29)) {
    pos(2) := UInt(30) + PriorityEncoder(Vec.tabulate(32-29-1) { i => !addrq(i+29+1).valid })
  } .elsewhen (pos(1) === UInt(30)) {
    pos(2) := UInt(31) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !addrq(i+30+1).valid })
  } .elsewhen (pos(1) === UInt(31)) {
    pos(2) := UInt(3) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !addrq(i+30+1).valid })
  } .otherwise {
    pos(2) := UInt(1) + PriorityEncoder(Vec.tabulate(32-0-1) { i => !addrq(i+0+1).valid })
  }

  when(pos(2) === UInt(1)) {
    pos(3) := UInt(2) + PriorityEncoder(Vec.tabulate(32-1-1) { i => !addrq(i+1+1).valid })
  } .elsewhen (pos(2) === UInt(2)) {
    pos(3) := UInt(3) + PriorityEncoder(Vec.tabulate(32-2-1) { i => !addrq(i+2+1).valid })
  } .elsewhen (pos(2) === UInt(3)) {
    pos(3) := UInt(4) + PriorityEncoder(Vec.tabulate(32-3-1) { i => !addrq(i+3+1).valid })
  } .elsewhen (pos(2) === UInt(4)) {
    pos(3) := UInt(5) + PriorityEncoder(Vec.tabulate(32-4-1) { i => !addrq(i+4+1).valid })
  } .elsewhen (pos(2) === UInt(5)) {
    pos(3) := UInt(6) + PriorityEncoder(Vec.tabulate(32-5-1) { i => !addrq(i+5+1).valid })
  } .elsewhen (pos(2) === UInt(6)) {
    pos(3) := UInt(7) + PriorityEncoder(Vec.tabulate(32-6-1) { i => !addrq(i+6+1).valid })
  } .elsewhen (pos(2) === UInt(7)) {
    pos(3) := UInt(8) + PriorityEncoder(Vec.tabulate(32-7-1) { i => !addrq(i+7+1).valid })
  } .elsewhen (pos(2) === UInt(8)) {
    pos(3) := UInt(9) + PriorityEncoder(Vec.tabulate(32-8-1) { i => !addrq(i+8+1).valid })
  } .elsewhen (pos(2) === UInt(9)) {
    pos(3) := UInt(10) + PriorityEncoder(Vec.tabulate(32-9-1) { i => !addrq(i+9+1).valid })
  } .elsewhen (pos(2) === UInt(10)) {
    pos(3) := UInt(11) + PriorityEncoder(Vec.tabulate(32-10-1) { i => !addrq(i+10+1).valid })
  } .elsewhen (pos(2) === UInt(11)) {
    pos(3) := UInt(12) + PriorityEncoder(Vec.tabulate(32-11-1) { i => !addrq(i+11+1).valid })
  } .elsewhen (pos(2) === UInt(12)) {
    pos(3) := UInt(13) + PriorityEncoder(Vec.tabulate(32-12-1) { i => !addrq(i+12+1).valid })
  } .elsewhen (pos(2) === UInt(13)) {
    pos(3) := UInt(14) + PriorityEncoder(Vec.tabulate(32-13-1) { i => !addrq(i+13+1).valid })
  } .elsewhen (pos(2) === UInt(14)) {
    pos(3) := UInt(15) + PriorityEncoder(Vec.tabulate(32-14-1) { i => !addrq(i+14+1).valid })
  } .elsewhen (pos(2) === UInt(15)) {
    pos(3) := UInt(16) + PriorityEncoder(Vec.tabulate(32-15-1) { i => !addrq(i+15+1).valid })
  } .elsewhen (pos(2) === UInt(16)) {
    pos(3) := UInt(17) + PriorityEncoder(Vec.tabulate(32-16-1) { i => !addrq(i+16+1).valid })
  } .elsewhen (pos(2) === UInt(17)) {
    pos(3) := UInt(18) + PriorityEncoder(Vec.tabulate(32-17-1) { i => !addrq(i+17+1).valid })
  } .elsewhen (pos(2) === UInt(18)) {
    pos(3) := UInt(19) + PriorityEncoder(Vec.tabulate(32-18-1) { i => !addrq(i+18+1).valid })
  } .elsewhen (pos(2) === UInt(19)) {
    pos(3) := UInt(20) + PriorityEncoder(Vec.tabulate(32-19-1) { i => !addrq(i+19+1).valid })
  } .elsewhen (pos(2) === UInt(20)) {
    pos(3) := UInt(21) + PriorityEncoder(Vec.tabulate(32-20-1) { i => !addrq(i+20+1).valid })
  } .elsewhen (pos(2) === UInt(21)) {
    pos(3) := UInt(22) + PriorityEncoder(Vec.tabulate(32-21-1) { i => !addrq(i+21+1).valid })
  } .elsewhen (pos(2) === UInt(22)) {
    pos(3) := UInt(23) + PriorityEncoder(Vec.tabulate(32-22-1) { i => !addrq(i+22+1).valid })
  } .elsewhen (pos(2) === UInt(23)) {
    pos(3) := UInt(24) + PriorityEncoder(Vec.tabulate(32-23-1) { i => !addrq(i+23+1).valid })
  } .elsewhen (pos(2) === UInt(24)) {
    pos(3) := UInt(25) + PriorityEncoder(Vec.tabulate(32-24-1) { i => !addrq(i+24+1).valid })
  } .elsewhen (pos(2) === UInt(25)) {
    pos(3) := UInt(26) + PriorityEncoder(Vec.tabulate(32-25-1) { i => !addrq(i+25+1).valid })
  } .elsewhen (pos(2) === UInt(26)) {
    pos(3) := UInt(27) + PriorityEncoder(Vec.tabulate(32-26-1) { i => !addrq(i+26+1).valid })
  } .elsewhen (pos(2) === UInt(27)) {
    pos(3) := UInt(28) + PriorityEncoder(Vec.tabulate(32-27-1) { i => !addrq(i+27+1).valid })
  } .elsewhen (pos(2) === UInt(28)) {
    pos(3) := UInt(29) + PriorityEncoder(Vec.tabulate(32-28-1) { i => !addrq(i+28+1).valid })
  } .elsewhen (pos(2) === UInt(29)) {
    pos(3) := UInt(30) + PriorityEncoder(Vec.tabulate(32-29-1) { i => !addrq(i+29+1).valid })
  } .elsewhen (pos(2) === UInt(30)) {
    pos(3) := UInt(31) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !addrq(i+30+1).valid })
  } .elsewhen (pos(2) === UInt(31)) {
    pos(3) := UInt(3) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !addrq(i+30+1).valid })
  } .otherwise {
    pos(3) := UInt(1) + PriorityEncoder(Vec.tabulate(32-0-1) { i => !addrq(i+0+1).valid })
  }

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
      when (WbCamAddr.io.hit(j)(i) && io.robWbin.valid_s1(j)
              && io.robWbin.is_addr_s1(j)) {
        addrq(i).bits.addr.valid := Bool(true)
        addrq(i).bits.addr.bits := io.robWbin.data_s1(j)
      } .elsewhen (WbCamAddr.io.hit(j+4)(i) && io.robWbin.valid_s2(j)
                    && io.robWbin.is_addr_s2(j)) {
        addrq(i).bits.addr.valid := Bool(true)
        addrq(i).bits.addr.bits := io.robWbin.data_s2(j)
      }
    }
    for ( j <- 0 until 6) {
      when (WbCamValue.io.hit(j)(i) && io.robWbin.valid_s1(j)
              && !io.robWbin.is_addr_s1(j) && addrq(i).bits.st_nld) {
        addrq(i).bits.value.valid := Bool(true)
        addrq(i).bits.value.bits := io.robWbin.data_s1(j)
      } .elsewhen (WbCamValue.io.hit(j+6)(i) && io.robWbin.valid_s2(j)
                    && !io.robWbin.is_addr_s2(j) && addrq(i).bits.st_nld) {
        addrq(i).bits.value.valid := Bool(true)
        addrq(i).bits.value.bits := io.robWbin.data_s2(j)
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
  val ldIssueRow = Vec(2, Vec(DEPTH, Bool()))
  val ldIssueSet = Vec(2, Bool())

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
            ldIssueRow(j)(i) := Bool(true)
            addrq(i).bits.addr.valid := Bool(false)
            addrq(i).bits.value.valid := Bool(false)
            addrq(i).valid := Bool(false)
          } 
          when (CamStCommit.io.hit(j)(i)) {
            printf("St dispatch on %d\n", UInt(j))
            printf("St dispatch addr %d\n", addrq(i).bits.addr.bits)
            printf("St dispatch value %d\n", addrq(i).bits.value.bits)
            addrq(i).bits.addr.valid := Bool(false)
            addrq(i).valid := Bool(false)
            stCommitRow(j)(i) := Bool(true)
          }
      } .otherwise {
        stCommitRow(j)(i) := Bool(false)
        ldIssueRow(j)(i) := Bool(false)
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

    ldIssueSet(i) := Cat(Array.tabulate(32) { ldIssueRow(i)(_) }).orR
    when(ldIssueSet(i)) {
      io.robWbOut.data(i) := addrq(PriorityEncoder(ldIssueRow(i))).bits.value.bits
      io.robWbOut.is_addr(i) := Bool(false)
      io.robWbOut.operand(i) := addrq(PriorityEncoder(ldIssueRow(i))).bits.robLoc
      io.robWbOut.valid(i) := Bool(true)
    } .otherwise {
      io.robWbOut.data(i) := UInt(0xdead)
      io.robWbOut.is_addr(i) := Bool(false)
      io.robWbOut.operand(i) := UInt(0x0)
      io.robWbOut.valid(i) := Bool(false)
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

  poke(c.io.robWbin.operand_s1(2), 6)
  poke(c.io.robWbin.is_addr_s1(2), true)
  poke(c.io.robWbin.valid_s1(2), true)
  poke(c.io.robWbin.data_s1(2), 0x10000)
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
  poke(c.io.robWbin.operand_s2(3), 6)
  poke(c.io.robWbin.is_addr_s2(3), false)
  poke(c.io.robWbin.valid_s2(3), true)
  poke(c.io.robWbin.data_s2(3), 0xff)
  expect(c.WbCamValue.io.hit(9)(0), true)
  //expect(c.addrq(0).bits.value.valid, false)
  poke(c.io.robWbin.operand_s1(1), 4)
  poke(c.io.robWbin.is_addr_s1(1), true)
  poke(c.io.robWbin.valid_s1(1), true)
  poke(c.io.robWbin.data_s1(1), 0x10000)
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
