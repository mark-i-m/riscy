package riscy

import Chisel._

object ALU {
  val SZ_ALU_FN = 4
  //val FN_X    = BitPat("b????")
  val FN_UNKNOWN = UInt(0)
  val FN_ADD  = UInt(1)
  val FN_SLL   = UInt(2)
  val FN_SEQ  = UInt(3)
  val FN_SNE  = UInt(4)
  val FN_XOR  = UInt(5)
  val FN_SRL   = UInt(6)
  val FN_OR   = UInt(7)
  val FN_AND  = UInt(8)
  val FN_SUB  = UInt(9)
  val FN_SRA  = UInt(10)
  val FN_SLT  = UInt(11)
  val FN_SGE  = UInt(12)
  val FN_SLTU = UInt(13)
  val FN_SGEU = UInt(14)

  val A1_RS1 = UInt(0)
  val A1_PC = UInt(1)

  val A2_RS2 = UInt(0)
  val A2_IMM_I = UInt(1)
  val A2_IMM_S = UInt(2)

  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd === FN_SEQ || cmd === FN_SNE || cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
}
import ALU._


class ALU(xLen : Int) extends Module {
  val io = new Bundle {
    val rs1_val = UInt(INPUT, xLen)
    val rs2_val = UInt(INPUT, xLen)
    val PC = UInt(INPUT, xLen)
    val inst = new DecodeIns().flip

    val out = UInt(OUTPUT, xLen)
    val adder_out = UInt(OUTPUT, xLen)
    val cmp_out = Bool(OUTPUT)
  }

  // Determine the function we need to perform
  val op = UInt(width=6)
  when (io.inst.op === UInt(0x33)) {
      // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
      when (io.inst.funct3 === UInt(0x0)) {
        when (io.inst.funct7 === UInt(0x0)) {
          op := FN_ADD
        } .elsewhen (io.inst.funct7 === UInt(0x20)) {
          op := FN_SUB
        }
      } .elsewhen (io.inst.funct3 === UInt(0x1)) {
        op := FN_SLL
      } .elsewhen (io.inst.funct3 === UInt(0x2)) {
        op := FN_SLT
      } .elsewhen (io.inst.funct3 === UInt(0x3)) {
        op := FN_SLTU
      } .elsewhen (io.inst.funct3 === UInt(0x4)) {
        op := FN_XOR
      } .elsewhen (io.inst.funct3 === UInt(0x5)) {
        when (io.inst.funct7 === UInt(0x0)) {
          op := FN_SRL
        } .elsewhen (io.inst.funct7 === UInt(0x20)) {
          op := FN_SRA
        }
      } .elsewhen (io.inst.funct3 === UInt(0x6)) {
        op := FN_OR
      } .elsewhen (io.inst.funct3 === UInt(0x7)) {
        op := FN_AND
      }

  } .elsewhen (io.inst.op === UInt(0x13)) {
      // ADDI, SLLI, SLTI, SLTIU, XORI, SRLI, SRAI, ORI, ANDI
      when (io.inst.funct3 === UInt(0x0)) {
        op := FN_ADD
      } .elsewhen (io.inst.funct3 === UInt(0x1)) {
        op := FN_SLL
      } .elsewhen (io.inst.funct3 === UInt(0x2)) {
        op := FN_SLT
      } .elsewhen (io.inst.funct3 === UInt(0x3)) {
        op := FN_SLTU
      } .elsewhen (io.inst.funct3 === UInt(0x4)) {
        op := FN_XOR
      } .elsewhen (io.inst.funct3 === UInt(0x5)) {
        when (io.inst.funct7 === UInt(0x0)) {
          op := FN_SRL
        } .elsewhen (io.inst.funct7 === UInt(0x20)) {
          op := FN_SRA
        }
      } .elsewhen (io.inst.funct3 === UInt(0x6)) {
        op := FN_OR
      } .elsewhen (io.inst.funct3 === UInt(0x7)) {
        op := FN_AND
      }
  } .elsewhen (io.inst.op === UInt(0x1B)) {
    // ADDIW, SLLIW, SRLIW, SRAIW
    when (io.inst.funct3 === UInt(0x0)) {
      op := FN_ADD
    } .elsewhen (io.inst.funct3 === UInt(0x1)) {
      op := FN_SLL
    } .elsewhen (io.inst.funct3 === UInt(0x5)) {
      when (io.inst.funct7 === UInt(0x0)) {
        op := FN_SRL
      } .elsewhen (io.inst.funct7 === UInt(0x20)) {
        op := FN_SRA
      }
    }
  } .elsewhen (io.inst.op === UInt(0x3B)) {
    // ADDW, SUBW, SLLW, SRLW, SRAW
    when (io.inst.funct3 === UInt(0x0)) {
      when (io.inst.funct7 === UInt(0x0)) {
        op := FN_ADD
      } .elsewhen (io.inst.funct7 === UInt(0x20)) {
        op := FN_SUB
      }
    } .elsewhen (io.inst.funct3 === UInt(0x1)) {
      op := FN_SLL
    } .elsewhen (io.inst.funct3 === UInt(0x5)) {
      when (io.inst.funct7 === UInt(0x0)) {
        op := FN_SRL
      } .elsewhen (io.inst.funct7 === UInt(0x20)) {
        op := FN_SRA
      }
    }
  } .elsewhen (io.inst.op === UInt(0x3)) {
    // LB, LH, LW, LBU, LHU
    op := FN_ADD
  } .elsewhen (io.inst.op === UInt(0x23)) {
    // SB, SH, SW
    op := FN_ADD
  } .otherwise {
    op := FN_UNKNOWN
  }

  // Selector logic for deciding which inputs must be chosen for the ALU to
  // perform the operation
  val sel_a1 = UInt(width=3)
  val sel_a2 = UInt(width=3)
  when (io.inst.op === UInt(0x33)) {
    // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
    sel_a1 := A1_RS1
    sel_a2 := A2_RS2
  } .elsewhen (io.inst.op === UInt(0x13)) {
    // ADDI, SLLI, SLTI, SLTIU, XORI, SRLI, SRAI, ORI, ANDI
    sel_a1 := A1_RS1
    sel_a2 := A2_IMM_I
  } .elsewhen (io.inst.op === UInt(0x1B)) {
    // ADDIW, SLLIW, SRLIW, SRAIW
    sel_a1 := A1_RS1
    sel_a2 := A2_IMM_I
  } .elsewhen (io.inst.op === UInt(0x3B)) {
    // ADDW, SUBW, SLLW, SRLW, SRAW
    sel_a1 := A1_RS1
    sel_a2 := A2_RS2
  } .elsewhen (io.inst.op === UInt(0x3)) {
    // LB, LH, LW, LBU, LHU
    sel_a1 := A1_RS1
    sel_a2 := A2_IMM_I
  } .elsewhen (io.inst.op === UInt(0x23)) {
    // SB, SH, SW
    sel_a1 := A1_RS1
    sel_a2 := A2_IMM_S
  } .otherwise {
    sel_a1 := A1_RS1
    sel_a2 := A2_RS2
  }

  // immI and immS are only 32 bits. Sign extend them before using
  val immI_ext = Cat(Fill(32, io.inst.immI(31)), io.inst.immI)
  val immS_ext = Cat(Fill(32, io.inst.immS(31)), io.inst.immS)

  // Select the inputs for ALU operations
  val in1 = MuxLookup(sel_a1, UInt(0), Seq(
    A1_RS1 -> io.rs1_val,
    A1_PC -> io.PC))
  val in2 = MuxLookup(sel_a2, UInt(0), Seq(
    A2_RS2 -> io.rs2_val,
    A2_IMM_I -> immI_ext.asUInt,
    A2_IMM_S -> immS_ext.asUInt))

  // ADD, SUB
  val in2_inv = Mux(op === FN_SUB, ~in2, in2)
  val in1_xor_in2 = in1 ^ in2_inv
  io.adder_out := in1 + in2_inv + UInt(op === FN_SUB)

  // SLT, SLTU
  io.cmp_out := Mux(op === FN_SLT, in1.asSInt < in2.asSInt, in1 < in2)
  val slt_out = UInt(width=xLen)
  slt_out := Mux(op === FN_SLT || op === FN_SLTU, io.cmp_out, UInt(0))

  // SLL, SRL, SRA
  val sh_amt = in2(5,0)
  val sh_out_l = (in1 << sh_amt)(xLen-1,0)
  // Sign bits are extended only for SRA
  val sh_out_r = (Cat(op === FN_SRA & in1(xLen-1),
                  in1(xLen-1,0)).asSInt >> sh_amt)(xLen-1,0)
  //val sh_out_r = (Cat(Bits(1), in1).asSInt >> sh_amt)
  val sh_out = Mux(op === FN_SRL || op === FN_SRA, sh_out_r, UInt(0)) |
               Mux(op === FN_SLL, sh_out_l, UInt(0))

  // AND, OR, XOR
  val logic = MuxLookup(op, UInt(0), Seq(
    FN_XOR -> in1_xor_in2,
    FN_OR -> UInt(in1(xLen-1,0) | in2(xLen-1,0)),
    FN_AND -> UInt(in1(xLen-1,0) & in2(xLen-1,0))))

  val non_arith_out = slt_out | logic | sh_out
  io.out := Mux(op === FN_ADD || op === FN_SUB, io.adder_out, non_arith_out)
}

class ALUTests(c: ALU) extends Tester(c) {
  // Used for populating opcodes, funct3 and funct7 values. Format is:
  // "INST_NAME" -> (opcode, funct3, funct7)
  val inst_map = Map(
    "ADD" -> (0x33, 0x0, 0x0),
    "SUB" -> (0x33, 0x0, 0x20),
    "SLL" -> (0x33, 0x1, 0x0),
    "SLT" -> (0x33, 0x2, 0x0),
    "SLTU" -> (0x33, 0x3, 0x0),
    "XOR" -> (0x33, 0x4, 0x0),
    "SRL" -> (0x33, 0x5, 0x0),
    "SRA" -> (0x33, 0x5, 0x20),
    "OR" -> (0x33, 0x6, 0x0),
    "AND" -> (0x33, 0x7, 0x0),

    "ADDI" -> (0x13, 0x0, 0x0),
    "SLLI" -> (0x13, 0x1, 0x0),
    "SLTI" -> (0x13, 0x2, 0x0),
    "SLTIU" -> (0x13, 0x3, 0x0),
    "XORI" -> (0x13, 0x4, 0x0),
    "SRLI" -> (0x13, 0x5, 0x0),
    "SRAI" -> (0x13, 0x5, 0x20),
    "ORI" -> (0x13, 0x6, 0x0),
    "ANDI" -> (0x13, 0x7, 0x0),

    "LB" -> (0x3, 0x0, 0x0),
    "LH" -> (0x3, 0x1, 0x0),
    "LW" -> (0x3, 0x2, 0x0),
    "LBU" -> (0x3, 0x3, 0x0),
    "LBH" -> (0x3, 0x4, 0x0),

    "SB" -> (0x23, 0x0, 0x0),
    "SH" -> (0x23, 0x1, 0x0),
    "SW" -> (0x23, 0x2, 0x0)
  )
  // Utility function for setting opcode, funct3 and funct7 values
  def set_instruction(inst_name : String) = {
    println("Testing " + inst_name + " instruction")
    val value = inst_map(inst_name)
    poke(c.io.inst.op, value._1)
    poke(c.io.inst.funct3, value._2)
    poke(c.io.inst.funct7, value._3)
  }

  // 1. Test ADD instruction - Sanity
  set_instruction("ADD")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 100)

  // 2. Test ADD instruction - 1 negative value
  set_instruction("ADD")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, -100L)
  step(1)
  expect(c.io.out, -50L)

  // 3. Test ADD instruction - 2 negative values
  set_instruction("ADD")
  poke(c.io.rs1_val, -100L)
  poke(c.io.rs2_val, -100L)
  step(1)
  expect(c.io.out, -200L)

  // 4. Test SUB instruction - Sanity
  set_instruction("SUB")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 0)

  // 5. Test SUB instruction - Sanity 2
  set_instruction("SUB")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  step(1)
  expect(c.io.out, -50L)

  // 6. Test SUB instruction - 1 negative value
  set_instruction("SUB")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, -50L)
  step(1)
  expect(c.io.out, 100)

  // 7. Test SUB instruction - 2 negative values
  set_instruction("SUB")
  poke(c.io.rs1_val, -50L)
  poke(c.io.rs2_val, 50L)
  step(1)
  expect(c.io.out, -100L)

  // 8. Test SLL instruction - sanity
  set_instruction("SLL")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 2)
  step(1)
  expect(c.io.out, 400)

  // 9. Test SLL instruction - Verify that only the 5 lower bits in rs2_val are
  // taken. 
  set_instruction("SLL")
  poke(c.io.rs1_val, 1)
  // The lower five bits are all zero for rs2_val. We expect the result to be
  // equal to rs1_val
  poke(c.io.rs2_val, 64)
  step(1)
  expect(c.io.out, 1)

  // 10. Test SLT instruction - sanity 1 (not less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 0)
  
  // 11. Test SLT instruction - sanity 2 (less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  step(1)
  expect(c.io.out, 1)

  // 12. Test SLT instruction - Signed comparison sanity 1 (not less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -2L)
  step(1)
  expect(c.io.out, 0)

  // 13. Test SLT instruction - Signed comparison sanity 2 (less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, -2L)
  poke(c.io.rs2_val, -1L)
  step(1)
  expect(c.io.out, 1)

  // 14. Test SLTU instruction - sanity 1 (not less than)
  set_instruction("SLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 0)
  
  // 15. Test SLTU instruction - sanity 2 (less than)
  set_instruction("SLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  step(1)
  expect(c.io.out, 1)

  // 16. Test SLTU instruction - sanity 3 (Pick a negative number. Should not
  // be considered as a signed value)
  set_instruction("SLTU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 0)
  step(1)
  expect(c.io.out, 0)

  // 17. Test XOR instruction 
  set_instruction("XOR")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.rs2_val, 234567890)
  step(1)
  expect(c.io.out, 123456789 ^ 234567890)

  // 18. Test SRL instruction - sanity
  set_instruction("SRL")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 50 >> 1)

  // 19. Test SRL instruction - Verify that only 5 lower bits of rs2_val are
  // considered.
  set_instruction("SRL")
  poke(c.io.rs1_val, 50)
  // The lower five bits are all zero for rs2_val. We expect the result to be
  // equal to rs1_val
  poke(c.io.rs2_val, 64)
  step(1)
  expect(c.io.out, 50)

  // 20. Test SRL instruction - Verify that sign extended bit is ignored
  set_instruction("SRL")
  // Set to -4 ie. 0xfffffffffffffffc
  poke(c.io.rs1_val, -4L)
  poke(c.io.rs2_val, 2)
  step(1)
  // Since SRL ignores the sign bit and just brings in zeroes, we expect it to
  // convert to 0x3fffffffffffffff
  expect(c.io.out, 0x3fffffffffffffffL)

  // 21. Test SRA instruction - Sanity
  set_instruction("SRA")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 50 >> 1)

  // 22. Test SRA instruction - Verify that sign extended bit is not ignored
  set_instruction("SRA")
  poke(c.io.rs1_val, -4L)
  poke(c.io.rs2_val, 2)
  step(1)
  expect(c.io.out, -1L)

  // 23. Test SRA instruction - Verify that only 5 lower bits of rs2_val are
  // considered.
  set_instruction("SRA")
  poke(c.io.rs1_val, 50)
  // The lower five bits are all zero for rs2_val. We expect the result to be
  // equal to rs1_val
  poke(c.io.rs2_val, 64)
  step(1)
  expect(c.io.out, 50)

  // 24. Test OR instruction 
  set_instruction("OR")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.rs2_val, 234567890)
  step(1)
  expect(c.io.out, 123456789 | 234567890)

  // 25. Test AND instruction 
  set_instruction("AND")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.rs2_val, 234567890)
  step(1)
  expect(c.io.out, 123456789 & 234567890)

  // 26. Test ADDI instruction - Sanity
  set_instruction("ADDI")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, 100)
  step(1)
  expect(c.io.out, 200)

  // 27. Test ADDI instruction - Negative IMMI value
  set_instruction("ADDI")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -200)
  step(1)
  expect(c.io.out, -100L)

  // 28. Test ADDI instruction - Both negative values
  set_instruction("ADDI")
  poke(c.io.rs1_val, -100L)
  poke(c.io.inst.immI, -200)
  step(1)
  expect(c.io.out, -300L)

  // 29. Test SLLI instruction - Sanity
  set_instruction("SLLI")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, 2)
  step(1)
  expect(c.io.out, 400)

  // 30. Test SLLI instruction - Verify that only the lower 5 bits in IMMI
  // value are considered
  set_instruction("SLLI")
  poke(c.io.rs1_val, 100)
  // The lower five bits are all zero for IMMI. We expect the result to be
  // equal to rs1_val
  poke(c.io.inst.immI, 64)
  step(1)
  expect(c.io.out, 100)

  // 31. Test SLTI instruction - sanity 1 (not less than)
  set_instruction("SLTI")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 0)
  
  // 32. Test SLTI instruction - sanity 2 (less than)
  set_instruction("SLTI")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 100)
  step(1)
  expect(c.io.out, 1)

  // 33. Test SLTI instruction - Signed comparison sanity 1 (not less than)
  set_instruction("SLTI")
  poke(c.io.rs1_val, -1L)
  poke(c.io.inst.immI, -2)
  step(1)
  expect(c.io.out, 0)

  // 34. Test SLT instruction - Signed comparison sanity 2 (less than)
  set_instruction("SLTI")
  poke(c.io.rs1_val, -2L)
  poke(c.io.inst.immI, -1)
  step(1)
  expect(c.io.out, 1)

  // 35. Test SLTIU instruction - sanity 1 (not less than)
  set_instruction("SLTIU")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 0)
  
  // 36. Test SLTIU instruction - sanity 2 (less than)
  set_instruction("SLTIU")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 100)
  step(1)
  expect(c.io.out, 1)

  // 37. Test SLTIU instruction - sanity 3 (Pick a negative number for rs1_val.
  // Should not be considered as a signed value)
  set_instruction("SLTIU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.inst.immI, 0)
  step(1)
  expect(c.io.out, 0)

  // 38. Test SLTIU instruction - sanity 4 (Pick a negative number for IMMI.
  // Should not be considered as a signed value)
  set_instruction("SLTIU")
  poke(c.io.rs1_val, 0)
  poke(c.io.inst.immI, -1)
  step(1)
  expect(c.io.out, 1)

  // 39. Test XORI instruction 
  set_instruction("XORI")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.inst.immI, 234567890)
  step(1)
  expect(c.io.out, 123456789 ^ 234567890)

  // 40. Test SRLI instruction - sanity
  set_instruction("SRLI")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 1)
  step(1)
  expect(c.io.out, 25)

  // 41. Test SRLI instruction - Verify that only 5 lower bits of IMMI are
  // considered.
  set_instruction("SRLI")
  poke(c.io.rs1_val, 50)
  // The lower five bits are all zero for IMMI. We expect the result to be
  // equal to rs1_val
  poke(c.io.inst.immI, 64)
  step(1)
  expect(c.io.out, 50)

  // 42. Test SRLI instruction - Verify that sign extended bit is ignored
  set_instruction("SRLI")
  // Set to -4 ie. 0xfffffffffffffffc
  poke(c.io.rs1_val, -4L)
  poke(c.io.inst.immI, 2)
  step(1)
  // Since SRL ignores the sign bit and just brings in zeroes, we expect it to
  // convert to 0x3fffffffffffffff
  expect(c.io.out, 0x3fffffffffffffffL)

  // 43. Test SRAI instruction - Sanity
  set_instruction("SRAI")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 1)
  step(1)
  expect(c.io.out, 25)

  // 44. Test SRAI instruction - Verify that sign extended bit is not ignored
  set_instruction("SRAI")
  poke(c.io.rs1_val, -4L)
  poke(c.io.inst.immI, 2)
  step(1)
  expect(c.io.out, -1L)

  // 45. Test SRAI instruction - Verify that only 5 lower bits of IMMI are
  // considered.
  set_instruction("SRAI")
  poke(c.io.rs1_val, 50)
  // The lower five bits are all zero for IMMI. We expect the result to be
  // equal to rs1_val
  poke(c.io.inst.immI, 64)
  step(1)
  expect(c.io.out, 50)

  // 46. Test ORI instruction 
  set_instruction("ORI")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.inst.immI, 234567890)
  step(1)
  expect(c.io.out, 123456789 | 234567890)

  // 47. Test ANDI instruction 
  set_instruction("ANDI")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.inst.immI, 234567890)
  step(1)
  expect(c.io.out, 123456789 & 234567890)

  // 49. Test LB instruction - Positive IMMI value
  set_instruction("LB")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 50. Test LB instruction - Negative IMMI value
  set_instruction("LB")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 49. Test LB instruction - Positive IMMI value
  set_instruction("LB")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 50. Test LB instruction - Negative IMMI value
  set_instruction("LB")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 51. Test LH instruction - Positive IMMI value
  set_instruction("LH")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 52. Test LH instruction - Negative IMMI value
  set_instruction("LH")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 53. Test LW instruction - Positive IMMI value
  set_instruction("LW")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 54. Test LW instruction - Negative IMMI value
  set_instruction("LW")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 55. Test LBU instruction - Positive IMMI value
  set_instruction("LBU")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 56. Test LBU instruction - Negative IMMI value
  set_instruction("LBU")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 57. Test LBH instruction - Positive IMMI value
  set_instruction("LBH")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 58. Test LBH instruction - Negative IMMI value
  set_instruction("LBH")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 59. Test SB instruction - Positive IMMS value
  set_instruction("SB")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immS, 50)
  step(1)
  expect(c.io.out, 100)

  // 60. Test SB instruction - Negative IMMS value
  set_instruction("SB")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immS, -50)
  step(1)
  expect(c.io.out, 50)

  // 61. Test SH instruction - Positive IMMS value
  set_instruction("SH")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immS, 50)
  step(1)
  expect(c.io.out, 100)

  // 62. Test SH instruction - Negative IMMS value
  set_instruction("SH")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immS, -50)
  step(1)
  expect(c.io.out, 50)

  // 63. Test SW instruction - Positive IMMS value
  set_instruction("SW")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immS, 50)
  step(1)
  expect(c.io.out, 100)

  // 64. Test SW instruction - Negative IMMS value
  set_instruction("SW")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immS, -50)
  step(1)
  expect(c.io.out, 50)
}

class ALUGenerator extends TestGenerator {
  def genMod(): Module = Module(new ALU(64))
  def genTest[T <: Module](c: T): Tester[T] =
    (new ALUTests(c.asInstanceOf[ALU])).asInstanceOf[Tester[T]]
}

