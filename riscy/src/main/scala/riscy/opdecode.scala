package riscy

import Chisel._

// TODO: In CSR registers Imm value is taken as CSR reg number so we have to decide
// how to handle it.
//
// TODO: System instructions have same opcode as CSR instructions, but they are
// not I-type and they are not taken care at this stage. Only difference
// between CSR and System is Funct3 (0 for system calls).
class OpDecode extends Bundle {
  val hasRs1    = Bool(OUTPUT)
  val hasRs2    = Bool(OUTPUT)
  val hasRd     = Bool(OUTPUT)
  val hasFunct3 = Bool(OUTPUT)
  val hasFunct7 = Bool(OUTPUT)
  val hasImmI   = Bool(OUTPUT)
  val hasImmS   = Bool(OUTPUT)
  val hasImmB   = Bool(OUTPUT)
  val hasImmU   = Bool(OUTPUT)
  val hasImmJ   = Bool(OUTPUT)
  val isLd      = Bool(OUTPUT)
  val isSt      = Bool(OUTPUT)

  // This signal is for emulation purposes
  val isHalt = Bool(OUTPUT)
}

class RiscyOpDecode extends Module {
  val io = new Bundle {
    // Input opcode (7 bits) from RiscyDecode module
    val op = UInt(INPUT, 7)
    // Output to RiscyAlloc for ROB entry generation
    val opInfo = new OpDecode()
  }

  when (io.op(1,0) === UInt(0x3)) { 
    when (io.op(6,2) === UInt(0x0C)  ||
          io.op(6,2) === UInt(0x0E)) {
      // R type - ADDW, SUBW, SLLW, SRLW, SRAW, ADD, SUB, SLL, SLT, SLTU, XOR,
      // SRL, SRA, OR, AND
      io.opInfo.hasRs1    := UInt(1)
      io.opInfo.hasRs2    := UInt(1)
      io.opInfo.hasRd     := UInt(1)
      io.opInfo.hasFunct3 := UInt(1)
      io.opInfo.hasFunct7 := UInt(1)
      io.opInfo.hasImmI   := UInt(0)
      io.opInfo.hasImmS   := UInt(0)
      io.opInfo.hasImmB   := UInt(0)
      io.opInfo.hasImmU   := UInt(0)
      io.opInfo.hasImmJ   := UInt(0)
    } .elsewhen (io.op(6,2) === UInt(0x04) ||
                 io.op(6,2) === UInt(0x00) ||
                 io.op(6,2) === UInt(0x19) ||
                 io.op(6,2) === UInt(0x06) ||
                 io.op(6,2) === UInt(0x1C)) {
      // I type - JALR, ADDI, SLLI, SLTI, SLTIU, XORI, SRLI, SRAI, ORI, ANDI,
      // ADDIW, SLLIW, SRLIW, SRAIW, LB, LH, LW, LD, LBU, LHU, LWU, CSRRW, CSRRS,
      // CSRRC, CSRRWI, CSRRSI, CSRRCI
      io.opInfo.hasRs1    := UInt(1)
      io.opInfo.hasRs2    := UInt(0)
      io.opInfo.hasRd     := UInt(1)
      io.opInfo.hasFunct3 := UInt(1)
      io.opInfo.hasFunct7 := UInt(1)
      io.opInfo.hasImmI   := UInt(1)
      io.opInfo.hasImmS   := UInt(0)
      io.opInfo.hasImmB   := UInt(0)
      io.opInfo.hasImmU   := UInt(0)
      io.opInfo.hasImmJ   := UInt(0)
    } .elsewhen (io.op(6,2) === UInt(0x08)) {
      // S type - SB, SH, SW, SD
      io.opInfo.hasRs1    := UInt(1)
      io.opInfo.hasRs2    := UInt(1)
      io.opInfo.hasRd     := UInt(0)
      io.opInfo.hasFunct3 := UInt(1)
      io.opInfo.hasFunct7 := UInt(0)
      io.opInfo.hasImmI   := UInt(0)
      io.opInfo.hasImmS   := UInt(1)
      io.opInfo.hasImmB   := UInt(0)
      io.opInfo.hasImmU   := UInt(0)
      io.opInfo.hasImmJ   := UInt(0)
    } .elsewhen (io.op(6,2) === UInt(0x18)) {
      // SB type - BEQ, BNE, BLT, BGE, BLTE, BGUE
      io.opInfo.hasRs1    := UInt(1)
      io.opInfo.hasRs2    := UInt(1)
      io.opInfo.hasRd     := UInt(0)
      io.opInfo.hasFunct3 := UInt(1)
      io.opInfo.hasFunct7 := UInt(0)
      io.opInfo.hasImmI   := UInt(0)
      io.opInfo.hasImmS   := UInt(0)
      io.opInfo.hasImmB   := UInt(1)
      io.opInfo.hasImmU   := UInt(0)
      io.opInfo.hasImmJ   := UInt(0)
    } .elsewhen (io.op(6,2) === UInt(0x0D) ||
                 io.op(6,2) === UInt(0x05)) {
      // U Type - LUI, AUIPC
      io.opInfo.hasRs1    := UInt(0)
      io.opInfo.hasRs2    := UInt(0)
      io.opInfo.hasRd     := UInt(1)
      io.opInfo.hasFunct3 := UInt(0)
      io.opInfo.hasFunct7 := UInt(0)
      io.opInfo.hasImmI   := UInt(0)
      io.opInfo.hasImmS   := UInt(0)
      io.opInfo.hasImmB   := UInt(0)
      io.opInfo.hasImmU   := UInt(1)
      io.opInfo.hasImmJ   := UInt(0)
    } .elsewhen (io.op(6,2) === UInt(0x1B)) {
      // UJ Type - JAL
      io.opInfo.hasRs1    := UInt(0)
      io.opInfo.hasRs2    := UInt(0)
      io.opInfo.hasRd     := UInt(1)
      io.opInfo.hasFunct3 := UInt(0)
      io.opInfo.hasFunct7 := UInt(0)
      io.opInfo.hasImmI   := UInt(0)
      io.opInfo.hasImmS   := UInt(0)
      io.opInfo.hasImmB   := UInt(0)
      io.opInfo.hasImmU   := UInt(0)
      io.opInfo.hasImmJ   := UInt(1)
    } .otherwise {
      io.opInfo.hasRs1    := UInt(0)
      io.opInfo.hasRs2    := UInt(0)
      io.opInfo.hasRd     := UInt(0)
      io.opInfo.hasFunct3 := UInt(0)
      io.opInfo.hasFunct7 := UInt(0)
      io.opInfo.hasImmI   := UInt(0)
      io.opInfo.hasImmS   := UInt(0)
      io.opInfo.hasImmB   := UInt(0)
      io.opInfo.hasImmU   := UInt(0)
      io.opInfo.hasImmJ   := UInt(0)
    }

    when (io.op(6,2) === UInt(0x08)) {
      io.opInfo.isSt := UInt(1)
    } .otherwise {
      io.opInfo.isSt := UInt(0)
    }

    when (io.op(6,2) === UInt(0x00)) {
      io.opInfo.isLd := UInt(1)
    } .otherwise {
      io.opInfo.isLd := UInt(0)
    }

    when (io.op === UInt(0x7F)) {
      io.opInfo.isHalt := Bool(true)
    } .otherwise {
      io.opInfo.isHalt := Bool(false)
    }
  } .otherwise {
    io.opInfo.hasRs1    := UInt(0)
    io.opInfo.hasRs2    := UInt(0)
    io.opInfo.hasRd     := UInt(0)
    io.opInfo.hasFunct3 := UInt(0)
    io.opInfo.hasFunct7 := UInt(0)
    io.opInfo.hasImmI   := UInt(0)
    io.opInfo.hasImmS   := UInt(0)
    io.opInfo.hasImmB   := UInt(0)
    io.opInfo.hasImmU   := UInt(0)
    io.opInfo.hasImmJ   := UInt(0)
    io.opInfo.isLd      := UInt(0)
    io.opInfo.isSt      := UInt(0)
    io.opInfo.isHalt    := UInt(0)
  }
}

class RiscyOpDecodeTests(c: RiscyOpDecode) extends Tester(c) {
  // R-type
  poke(c.io.op, 0x33)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, true)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, true)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  poke(c.io.op, 0x3B)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, true)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, true)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  // I-type
  poke(c.io.op, 0x13)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, true)
  expect(c.io.opInfo.hasImmI, true)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  poke(c.io.op, 0x03)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, true)
  expect(c.io.opInfo.hasImmI, true)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  poke(c.io.op, 0x67)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, true)
  expect(c.io.opInfo.hasImmI, true)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  poke(c.io.op, 0x1B)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, true)
  expect(c.io.opInfo.hasImmI, true)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  poke(c.io.op, 0x73)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, true)
  expect(c.io.opInfo.hasImmI, true)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  // S type
  poke(c.io.op, 0x23)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, true)
  expect(c.io.opInfo.hasRd,  false)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, false)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, true)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  // SB type
  poke(c.io.op, 0x63)
  step(1)
  expect(c.io.opInfo.hasRs1, true)
  expect(c.io.opInfo.hasRs2, true)
  expect(c.io.opInfo.hasRd,  false)
  expect(c.io.opInfo.hasFunct3, true)
  expect(c.io.opInfo.hasFunct7, false)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, true)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  // U type
  poke(c.io.op, 0x37)
  step(1)
  expect(c.io.opInfo.hasRs1, false)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, false)
  expect(c.io.opInfo.hasFunct7, false)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, true)
  expect(c.io.opInfo.hasImmJ, false)
  
  poke(c.io.op, 0x17)
  step(1)
  expect(c.io.opInfo.hasRs1, false)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, false)
  expect(c.io.opInfo.hasFunct7, false)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, true)
  expect(c.io.opInfo.hasImmJ, false)

  // J type
  poke(c.io.op, 0x6F)
  step(1)
  expect(c.io.opInfo.hasRs1, false)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  true)
  expect(c.io.opInfo.hasFunct3, false)
  expect(c.io.opInfo.hasFunct7, false)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, true)
  
  // Unvalid instructions
  poke(c.io.op, 0x30)
  step(1)
  expect(c.io.opInfo.hasRs1, false)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  false)
  expect(c.io.opInfo.hasFunct3, false)
  expect(c.io.opInfo.hasFunct7, false)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)

  poke(c.io.op, 0x07)
  step(1)
  expect(c.io.opInfo.hasRs1, false)
  expect(c.io.opInfo.hasRs2, false)
  expect(c.io.opInfo.hasRd,  false)
  expect(c.io.opInfo.hasFunct3, false)
  expect(c.io.opInfo.hasFunct7, false)
  expect(c.io.opInfo.hasImmI, false)
  expect(c.io.opInfo.hasImmS, false)
  expect(c.io.opInfo.hasImmB, false)
  expect(c.io.opInfo.hasImmU, false)
  expect(c.io.opInfo.hasImmJ, false)


}

class OpDecodeGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyOpDecode())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyOpDecodeTests(c.asInstanceOf[RiscyOpDecode])).asInstanceOf[Tester[T]]
}
