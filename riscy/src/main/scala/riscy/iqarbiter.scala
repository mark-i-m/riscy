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
  val era = UInt(OUTPUT, 7)
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
  val min01 = UInt(width = 2)
  val min02 = UInt(width = 2)
  val max01 = UInt(width = 2)
  val max02 = UInt(width = 2)
  val min11 = UInt(width = 2)
  val min12 = UInt(width = 2)
  val max11 = UInt(width = 2)
  val max12 = UInt(width = 2)
  val min21 = UInt(width = 2)
  val min22 = UInt(width = 2)
  val max21 = UInt(width = 2)
  val max22 = UInt(width = 2)
  val min31 = UInt(width = 2)
  val min32 = UInt(width = 2)
  val max31 = UInt(width = 2)
  val max32 = UInt(width = 2)
  val finalMin = Vec.fill(4) {UInt(width = 2)}

  // below logic is sorting all four iq lengths
  // I have written nested loop but it was not working
  // also we have to follow this method of sorting as
  // we can not increment queue lengths on the go without losing a cycle

  val minIncr0 = Vec.fill(4) {UInt(width = 2)}
  val minIncr1 = Vec.fill(4) {UInt(width = 2)}
  val minIncr2 = Vec.fill(4) {UInt(width = 2)}
  val minIncr3 = Vec.fill(4) {UInt(width = 2)}
  for (i <- 0 until 4) {
    minIncr0(i) := UInt(0)
  }

  when ((io.iqLen(0) + minIncr0(0)) <= (io.iqLen(1) + minIncr0(1))) {
    min01 := UInt(0)
    max01 := UInt(1)
  } .otherwise {
    min01 := UInt(1)
    max01 := UInt(0)
  }
  when ((io.iqLen(2) + minIncr0(2)) <= (io.iqLen(3) + minIncr0(3))) {
    min02 := UInt(2)
    max02 := UInt(3)
  } .otherwise {
    min02 := UInt(3)
    max02 := UInt(2)
  }
  when ((io.iqLen(min01) + minIncr0(min01)) <=
              (io.iqLen(min02) + minIncr0(min02))) {
    finalMin(0) := min01
  } .otherwise {
    finalMin(0) := min02
  }

  when (finalMin(0) === UInt(0)) {
    minIncr1(0) := minIncr0(0) + UInt(1)
    minIncr1(1) := minIncr0(1) + UInt(0)
    minIncr1(2) := minIncr0(2) + UInt(0)
    minIncr1(3) := minIncr0(3) + UInt(0)
  } .elsewhen (finalMin(0) === UInt(1)) {
    minIncr1(0) := minIncr0(0) + UInt(0)
    minIncr1(1) := minIncr0(1) + UInt(1)
    minIncr1(2) := minIncr0(2) + UInt(0)
    minIncr1(3) := minIncr0(3) + UInt(0)
  } .elsewhen (finalMin(0) === UInt(2)) {
    minIncr1(0) := minIncr0(0) + UInt(0)
    minIncr1(1) := minIncr0(1) + UInt(0)
    minIncr1(2) := minIncr0(2) + UInt(1)
    minIncr1(3) := minIncr0(3) + UInt(0)
  } .otherwise {
    minIncr1(0) := minIncr0(0) + UInt(0)
    minIncr1(1) := minIncr0(1) + UInt(0)
    minIncr1(2) := minIncr0(2) + UInt(0)
    minIncr1(3) := minIncr0(3) + UInt(1)
  }

  when ((io.iqLen(0) + minIncr1(0)) <= (io.iqLen(1) + minIncr1(1))) {
    min11 := UInt(0)
    max11 := UInt(1)
  } .otherwise {
    min11 := UInt(1)
    max11 := UInt(0)
  }
  when ((io.iqLen(2) + minIncr1(2)) <= (io.iqLen(3) + minIncr1(3))) {
    min12 := UInt(2)
    max12 := UInt(3)
  } .otherwise {
    min12 := UInt(3)
    max12 := UInt(2)
  }
  when ((io.iqLen(min11) + minIncr1(min11)) <=
              (io.iqLen(min12) + minIncr1(min12))) {
    finalMin(1) := min11
  } .otherwise {
    finalMin(1) := min12
  }

  when (finalMin(1) === UInt(0)) {
    minIncr2(0) := minIncr1(0) + UInt(1)
    minIncr2(1) := minIncr1(1) + UInt(0)
    minIncr2(2) := minIncr1(2) + UInt(0)
    minIncr2(3) := minIncr1(3) + UInt(0)
  } .elsewhen (finalMin(1) === UInt(1)) {
    minIncr2(0) := minIncr1(0) + UInt(0)
    minIncr2(1) := minIncr1(1) + UInt(1)
    minIncr2(2) := minIncr1(2) + UInt(0)
    minIncr2(3) := minIncr1(3) + UInt(0)
  } .elsewhen (finalMin(1) === UInt(2)) {
    minIncr2(0) := minIncr1(0) + UInt(0)
    minIncr2(1) := minIncr1(1) + UInt(0)
    minIncr2(2) := minIncr1(2) + UInt(1)
    minIncr2(3) := minIncr1(3) + UInt(0)
  }   .otherwise {
    minIncr2(0) := minIncr1(0) + UInt(0)
    minIncr2(1) := minIncr1(1) + UInt(0)
    minIncr2(2) := minIncr1(2) + UInt(0)
    minIncr2(3) := minIncr1(3) + UInt(1)
  }

  when ((io.iqLen(0) + minIncr2(0)) <= (io.iqLen(1) + minIncr2(1))) {
    min21 := UInt(0)
    max21 := UInt(1)
  } .otherwise {
    min21 := UInt(1)
    max21 := UInt(0)
  }
  when ((io.iqLen(2) + minIncr2(2)) <= (io.iqLen(3) + minIncr2(3))) {
    min22 := UInt(2)
    max22 := UInt(3)
  } .otherwise {
    min22 := UInt(3)
    max22 := UInt(2)
  }
  when ((io.iqLen(min21) + minIncr2(min21)) <=
              (io.iqLen(min22) + minIncr2(min22))) {
    finalMin(2) := min21
  } .otherwise {
    finalMin(2) := min22
  }

  when (finalMin(2) === UInt(0)) {
    minIncr3(0) := minIncr2(0) + UInt(1)
    minIncr3(1) := minIncr2(1) + UInt(0)
    minIncr3(2) := minIncr2(2) + UInt(0)
    minIncr3(3) := minIncr2(3) + UInt(0)
  } .elsewhen (finalMin(2) === UInt(1)) {
    minIncr3(0) := minIncr2(0) + UInt(0)
    minIncr3(1) := minIncr2(1) + UInt(1)
    minIncr3(2) := minIncr2(2) + UInt(0)
    minIncr3(3) := minIncr2(3) + UInt(0)
  } .elsewhen (finalMin(2) === UInt(2)) {
    minIncr3(0) := minIncr2(0) + UInt(0)
    minIncr3(1) := minIncr2(1) + UInt(0)
    minIncr3(2) := minIncr2(2) + UInt(1)
    minIncr3(3) := minIncr2(3) + UInt(0)
  } .otherwise {
    minIncr3(0) := minIncr2(0) + UInt(0)
    minIncr3(1) := minIncr2(1) + UInt(0)
    minIncr3(2) := minIncr2(2) + UInt(0)
    minIncr3(3) := minIncr2(3) + UInt(1)
  }

  when ((io.iqLen(0) + minIncr3(0)) <= (io.iqLen(1) + minIncr3(1))) {
    min31 := UInt(0)
    max31 := UInt(1)
  } .otherwise {
    min31 := UInt(1)
    max31 := UInt(0)
  }
  when ((io.iqLen(2) + minIncr3(2)) <= (io.iqLen(3) + minIncr3(3))) {
    min32 := UInt(2)
    max32 := UInt(3)
  } .otherwise {
    min32 := UInt(3)
    max32 := UInt(2)
  }
  when ((io.iqLen(min31) + minIncr3(min31)) <=
              (io.iqLen(min32) + minIncr3(min32))) {
    finalMin(3) := min31
  } .otherwise {
    finalMin(3) := min32
  }

  for (i <- 0 until 4) {
    when (!io.stall) {
      io.allocIQ(i).inst.bits := io.inst(i).bits
      io.allocIQ(i).iqNum := finalMin(i)
      io.allocIQ(i).inst.valid := io.inst(i).valid

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

      io.addrBuf(i).bits.robLoc       := io.inst(i).bits.tag
      io.addrBuf(i).bits.funct3       := io.inst(i).bits.funct3
      io.addrBuf(i).bits.rs1Rename    := io.inst(i).bits.rs1Rename
      io.addrBuf(i).bits.rs1Val       := io.inst(i).bits.rs1Val
      io.addrBuf(i).bits.rs2Rename    := io.inst(i).bits.rs2Rename
      io.addrBuf(i).bits.rs2Val       := io.inst(i).bits.rs2Val
    } .otherwise {
      io.allocIQ(i).inst.bits := io.inst(i).bits
      io.allocIQ(i).iqNum := finalMin(i)
      io.allocIQ(i).inst.valid := Bool(false)
      io.addrBuf(i).valid := Bool(false)
      io.addrBuf(i).bits.st_nld := Bool(false)
      io.addrBuf(i).bits.robLoc       := io.inst(i).bits.tag
      io.addrBuf(i).bits.funct3       := io.inst(i).bits.funct3
      io.addrBuf(i).bits.rs1Rename    := io.inst(i).bits.rs1Rename
      io.addrBuf(i).bits.rs1Val       := io.inst(i).bits.rs1Val
      io.addrBuf(i).bits.rs2Rename    := io.inst(i).bits.rs2Rename
      io.addrBuf(i).bits.rs2Val       := io.inst(i).bits.rs2Val
      io.addrBuf(i).bits.era          := io.inst(i).bits.era
    }
  }

  // pipeline the stall

  //val pipeStall = Reg (next = io.stall)



//  for (i <- 0 until 4) {
//      when (!io.stall) {
//          io.allocIQ(i).inst.bits := io.inst(i).bits
//          io.allocIQ(i).inst.valid := io.inst(i).valid
//
//          // Logic to issue instructions to different iqs
//          // if there is a stall signal set, top issue module
//          // should not issue any instructions to any iqs
//          when (UInt(i) <= io.iqLen(finalMin(1)) -
//                      (io.iqLen(finalMin(0)) + minIncr(0)(i))) {
//              io.allocIQ(i).iqNum := finalMin(0)
//              minIncr(0)(i+1) = minIncr(0)(i) + UInt(1)
//          } .elsewhen


//          when (UInt(i) <= minDiff(0)) {
//              io.allocIQ(i).iqNum := finalMin(0)
//          } .elsewhen (UInt(i) <= (minDiff(1) + minDiff(0) + UInt(1))) {
//              io.allocIQ(i).iqNum := finalMin(1)
//          } .elsewhen (UInt(i) <= (minDiff(2) + minDiff(1) + minDiff(0) + UInt(2))) {
//              io.allocIQ(i).iqNum := finalMin(2)
//          } .otherwise {
//              io.allocIQ(i).iqNum := finalMin(3)
//          }

            // Logic to generate the entry for LS Buffer
//          when ((io.inst(i).valid === Bool(true)) &&
//                      (io.inst(i).bits.isLd === Bool(true))) {
//              io.addrBuf(i).valid := Bool(true)
//              io.addrBuf(i).bits.st_nld := Bool(false)
//          } .elsewhen ((io.inst(i).valid === Bool(true)) &&
//                                   (io.inst(i).bits.isSt === Bool(true))) {
//              io.addrBuf(i).valid := Bool(true)
//              io.addrBuf(i).bits.st_nld := Bool(true)
//          } .otherwise {
//              io.addrBuf(i).valid := Bool(false)
//              io.addrBuf(i).bits.st_nld := Bool(false)
//          }
//          io.addrBuf(i).bits.robLoc       := io.inst(i).bits.tag
//          io.addrBuf(i).bits.funct3       := io.inst(i).bits.funct3
//          io.addrBuf(i).bits.rs1Rename    := io.inst(i).bits.rs1Rename
//          io.addrBuf(i).bits.rs1Val       := io.inst(i).bits.rs1Val
//          io.addrBuf(i).bits.rs2Rename    := io.inst(i).bits.rs2Rename
//          io.addrBuf(i).bits.rs2Val       := io.inst(i).bits.rs2Val
//      } .otherwise {
//
//          // Setting all values to false is stall is set
//          io.allocIQ(i).inst.bits := io.inst(i).bits
//          io.allocIQ(i).inst.valid := Bool(false)
//          io.allocIQ(i).iqNum := finalMin(0)
//          io.addrBuf(i).valid := Bool(false)
//          io.addrBuf(i).bits.st_nld := Bool(false)
//          io.addrBuf(i).bits.robLoc       := io.inst(i).bits.tag
//          io.addrBuf(i).bits.funct3       := io.inst(i).bits.funct3
//          io.addrBuf(i).bits.rs1Rename    := io.inst(i).bits.rs1Rename
//          io.addrBuf(i).bits.rs1Val       := io.inst(i).bits.rs1Val
//          io.addrBuf(i).bits.rs2Rename    := io.inst(i).bits.rs2Rename
//          io.addrBuf(i).bits.rs2Val       := io.inst(i).bits.rs2Val
//      }
//  }
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
  expect(c.io.allocIQ(3).iqNum, 0x0)
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
