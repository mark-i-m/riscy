package riscy

import Chisel._

object ALU {
  val SZ_ALU_FN = 4
  //val FN_X    = BitPat("b????")
  val FN_UNKNOWN = UInt(0)
  val FN_ADD  = UInt(1)
  val FN_SLL   = UInt(2)
  val FN_XOR  = UInt(3)
  val FN_SRL   = UInt(4)
  val FN_SRA  = UInt(5)
  val FN_OR   = UInt(6)
  val FN_AND  = UInt(7)

  val FN_SUB  = UInt(8)
  val FN_EQ   = UInt(9)
  val FN_NEQ  = UInt(10)
  val FN_SLT  = UInt(11)
  val FN_SGE  = UInt(12)
  val FN_SLTU = UInt(13)
  val FN_SGEU = UInt(14)

  // ALU Input 1 selection options
  val A1_RS1    = UInt(0)
  val A1_RS2    = UInt(1)
  val A1_PC     = UInt(2)
  val A1_ZERO   = UInt(3)
  val A1_RS1_32 = UInt(4)

  // ALU Input 2 selection options
  val A2_RS2    = UInt(0)
  val A2_RS1    = UInt(1)
  val A2_IMM_I  = UInt(2)
  val A2_IMM_S  = UInt(3)
  val A2_IMM_B  = UInt(4)
  val A2_IMM_J  = UInt(5)
  val A2_IMM_U  = UInt(6)
  val A2_RS2_32 = UInt(7)

  // Instructions needing the subtractor: UInt(8) - UInt(15)
  def isSub(cmd: UInt) = cmd(3)

  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
}
import ALU._

class ALU(xLen : Int) extends Module {
  val io = new Bundle {
    // The operand data values to be provided as input
    val rs1_val = UInt(INPUT, xLen)
    val rs2_val = UInt(INPUT, xLen)
    // The PC of the instruction to be executed
    val PC = UInt(INPUT, xLen)
    // The decoded information of the current instruction
    val inst = new DecodeIns().flip

    // The output of the ALU. Can be a 64-bit data value or a 64-bit address
    // depending on the type of the instruction
    val out = UInt(OUTPUT, xLen)
    // Output of the branch condition. Useful only for branch instructions
    val cmp_out = Bool(OUTPUT)
  }

  // Determine the function the ALU needs to perform
  val fn = UInt(width=6)
  fn := FN_UNKNOWN
  when (io.inst.op === UInt(0x33)) {
      // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
      when (io.inst.funct3 === UInt(0x0)) {
        when (io.inst.funct7 === UInt(0x0)) {
          fn := FN_ADD
        } .elsewhen (io.inst.funct7 === UInt(0x20)) {
          fn := FN_SUB
        }
      } .elsewhen (io.inst.funct3 === UInt(0x1)) {
        fn := FN_SLL
      } .elsewhen (io.inst.funct3 === UInt(0x2)) {
        fn := FN_SLT
      } .elsewhen (io.inst.funct3 === UInt(0x3)) {
        fn := FN_SLTU
      } .elsewhen (io.inst.funct3 === UInt(0x4)) {
        fn := FN_XOR
      } .elsewhen (io.inst.funct3 === UInt(0x5)) {
        when (io.inst.funct7 === UInt(0x0)) {
          fn := FN_SRL
        } .elsewhen (io.inst.funct7 === UInt(0x20)) {
          fn := FN_SRA
        }
      } .elsewhen (io.inst.funct3 === UInt(0x6)) {
        fn := FN_OR
      } .elsewhen (io.inst.funct3 === UInt(0x7)) {
        fn := FN_AND
      }

  } .elsewhen (io.inst.op === UInt(0x13)) {
      // ADDI, SLLI, SLTI, SLTIU, XORI, SRLI, SRAI, ORI, ANDI
      when (io.inst.funct3 === UInt(0x0)) {
        fn := FN_ADD
      } .elsewhen (io.inst.funct3 === UInt(0x1)) {
        fn := FN_SLL
      } .elsewhen (io.inst.funct3 === UInt(0x2)) {
        fn := FN_SLT
      } .elsewhen (io.inst.funct3 === UInt(0x3)) {
        fn := FN_SLTU
      } .elsewhen (io.inst.funct3 === UInt(0x4)) {
        fn := FN_XOR
      } .elsewhen (io.inst.funct3 === UInt(0x5)) {
        when (io.inst.funct7 === UInt(0x0)) {
          fn := FN_SRL
        } .elsewhen (io.inst.funct7 === UInt(0x20)) {
          fn := FN_SRA
        }
      } .elsewhen (io.inst.funct3 === UInt(0x6)) {
        fn := FN_OR
      } .elsewhen (io.inst.funct3 === UInt(0x7)) {
        fn := FN_AND
      }
  } .elsewhen (io.inst.op === UInt(0x1B)) {
    // ADDIW, SLLIW, SRLIW, SRAIW
    when (io.inst.funct3 === UInt(0x0)) {
      fn := FN_ADD
    } .elsewhen (io.inst.funct3 === UInt(0x1)) {
      fn := FN_SLL
    } .elsewhen (io.inst.funct3 === UInt(0x5)) {
      when (io.inst.funct7 === UInt(0x0)) {
        fn := FN_SRL
      } .elsewhen (io.inst.funct7 === UInt(0x20)) {
        fn := FN_SRA
      }
    }
  } .elsewhen (io.inst.op === UInt(0x3B)) {
    // ADDW, SUBW, SLLW, SRLW, SRAW
    when (io.inst.funct3 === UInt(0x0)) {
      when (io.inst.funct7 === UInt(0x0)) {
        fn := FN_ADD
      } .elsewhen (io.inst.funct7 === UInt(0x20)) {
        fn := FN_SUB
      }
    } .elsewhen (io.inst.funct3 === UInt(0x1)) {
      fn := FN_SLL
    } .elsewhen (io.inst.funct3 === UInt(0x5)) {
      when (io.inst.funct7 === UInt(0x0)) {
        fn := FN_SRL
      } .elsewhen (io.inst.funct7 === UInt(0x20)) {
        fn := FN_SRA
      }
    }
  } .elsewhen (io.inst.op === UInt(0x3)) {
    // RV32I: LB, LH, LW, LBU, LHU
    // RV64I: LWU, LD
    when (io.inst.funct3 != UInt(0x7)) {
      fn := FN_ADD
    }
  } .elsewhen (io.inst.op === UInt(0x23)) {
    // RV32I: SB, SH, SW
    // RV64I: SD
    when (!io.inst.funct3(2)) {
      // Funct3 values should be between 0x0 to 0x3. The third bit must be zero
      fn := FN_ADD
    }
  } .elsewhen (io.inst.op === UInt(0x63)) {
    // BEQ, BNE, BLT, BGE, BLTU, BGEU
    when (io.inst.funct3 === UInt(0x0)) {
      fn := FN_EQ
    } .elsewhen (io.inst.funct3 === UInt(0x1)) {
      fn := FN_NEQ
    } .elsewhen (io.inst.funct3 === UInt(0x4)) {
      fn := FN_SLT
    } .elsewhen (io.inst.funct3 === UInt(0x5)) {
      fn := FN_SGE
    } .elsewhen (io.inst.funct3 === UInt(0x6)) {
      fn := FN_SLTU
    } .elsewhen (io.inst.funct3 === UInt(0x7)) {
      fn := FN_SGEU
    }
  } .elsewhen (io.inst.op === UInt(0x6F)) {
    // JAL
    fn := FN_ADD
  } .elsewhen (io.inst.op === UInt(0x67)) {
    // JALR
    fn := FN_ADD
  } .elsewhen (io.inst.op === UInt(0x17)) {
    // AUIPC
    fn := FN_ADD
  } .elsewhen (io.inst.op === UInt(0x37)) {
    // LUI
    fn := FN_ADD
  } .otherwise {
    fn := FN_UNKNOWN
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
    sel_a1 := A1_RS1_32
    sel_a2 := A2_IMM_I
  } .elsewhen (io.inst.op === UInt(0x3B)) {
    // ADDW, SUBW, SLLW, SRLW, SRAW
    sel_a1 := A1_RS1_32
    sel_a2 := A2_RS2_32
  } .elsewhen (io.inst.op === UInt(0x3)) {
    // LB, LH, LW, LBU, LHU
    sel_a1 := A1_RS1
    sel_a2 := A2_IMM_I
  } .elsewhen (io.inst.op === UInt(0x23)) {
    // SB, SH, SW
    sel_a1 := A1_RS1
    sel_a2 := A2_IMM_S
  } .elsewhen (io.inst.op === UInt(0x63)) {
    // BEQ, BNE, BLT, BGE, BLTU, BGEU
    when (io.inst.funct3 === UInt(0x5) |
          io.inst.funct3 === UInt(0x7)) {
      // We reverse the operands for BGE and BGEU to derive results from BLT
      // and BLTU
      sel_a1 := A1_RS2
      sel_a2 := A2_RS1
    } .otherwise {
      sel_a1 := A1_RS1
      sel_a2 := A2_RS2
    }
  } .elsewhen (io.inst.op === UInt(0x6F)) {
    // JAL
    sel_a1 := A1_PC
    sel_a2 := A2_IMM_J
  } .elsewhen (io.inst.op === UInt(0x67)) {
    // JALR
    sel_a1 := A1_PC
    sel_a2 := A2_IMM_I
  } .elsewhen (io.inst.op === UInt(0x17)) {
    // AUIPC
    sel_a1 := A1_PC
    sel_a2 := A2_IMM_U
  } .elsewhen (io.inst.op === UInt(0x37)) {
    // LUI
    sel_a1 := A1_ZERO
    sel_a2 := A2_IMM_U
  } .otherwise {
    sel_a1 := A1_RS1
    sel_a2 := A2_RS2
  }

  // Immediate values are only 32 bits. Sign extend them before using
  val immI_ext = Cat(Fill(32, io.inst.immI(31)), io.inst.immI)
  val immS_ext = Cat(Fill(32, io.inst.immS(31)), io.inst.immS)
  val immB_ext = Cat(Fill(32, io.inst.immB(31)), io.inst.immB)
  val immJ_ext = Cat(Fill(32, io.inst.immJ(31)), io.inst.immJ)
  val immU_ext = Cat(Fill(32, io.inst.immU(31)), io.inst.immU)

  // Select the inputs for ALU operations
  val in1 = MuxLookup(sel_a1, UInt(0), Seq(
    A1_RS1    -> io.rs1_val,
    A1_RS2    -> io.rs2_val,
    A1_PC     -> io.PC,
    A1_ZERO   -> UInt(0, width=xLen),
    A1_RS1_32 -> Cat(Fill(32, io.rs1_val(31)), io.rs1_val(31,0)) ))

  // We are interested in putting 20 bits of immU followed by 12 bits of zeroes
  // to get 32-bit values for LUI and AUIPC. These values are then
  // sign-extended to 64 bits before getting used.
  val in2 = MuxLookup(sel_a2, UInt(0), Seq(
    A2_RS2    -> io.rs2_val,
    A2_RS2_32 -> Cat(Fill(32, io.rs1_val(31)), io.rs2_val(31,0)),
    A2_RS1    -> io.rs1_val,
    A2_IMM_I  -> immI_ext.asUInt,
    A2_IMM_S  -> immS_ext.asUInt,
    A2_IMM_B  -> immB_ext.asUInt,
    A2_IMM_J  -> immJ_ext.asUInt,
    A2_IMM_U  -> Cat(Cat(Fill(32, immU_ext(19)), immU_ext.asUInt()(19,0)),
                    Fill(12, UInt(0))) ))

  // ADD, SUB - Use an adder to derive results
  // LB, LH, LW, LBU, LHU, SB, SH, SW - Use adder to compute addresses
  // JAL, JALR - Use adder to compute target address (offset + PC + 4)
  // LUI, AUIPC
  val adder_out = UInt(width=xLen)
  val final_adder_out = UInt(width=xLen)
  val in2_inv = Mux(isSub(fn), ~in2, in2)
  val in1_xor_in2 = in1 ^ in2
  val isJmp = io.inst.op === UInt(0x6F) || io.inst.op === UInt(0x67)
  adder_out := (in1 + in2_inv +
                Mux(isSub(fn), UInt(1), Mux(isJmp, UInt(4), UInt(0))))
  when (io.inst.op === UInt(0x3B) |
        io.inst.op === UInt(0x1B)) {
    // ADDW, SUBW, ADDIW instructions - Need to consider only 32 bits of the
    // result and sign-extend it
    final_adder_out := Cat(Fill(32, adder_out(31)), adder_out(31,0))
  } .otherwise {
    final_adder_out := adder_out
  }

  // Use another adder for computing branch target since the prev adder will be
  // busy doing the subtraction for checking the condition
  // BEQ, BNE, BLT, BLTU, BGE, BGEU
  val branch_addr_out = UInt(width=xLen)
  branch_addr_out := io.PC + immB_ext.asUInt

  // SLT, SLTU, BEQ, BNE, BLT, BLTU, BGE, BGEU
  // These instructions rely on the subtraction result on the Adder above
  //
  // NOTE - We reuse the logic for BLT and BLTU for deriving results for BGE
  // and BGEU, by simply reversing the operands
  val in1_lt_in2 = Mux(in1(xLen-1) === in2(xLen-1), adder_out(xLen-1), in1(xLen-1))
  val in1_ltu_in2 = Mux(in1(xLen-1) === in2(xLen-1), adder_out(xLen-1), in2(xLen-1))

  // Select comparator output based on the ALU function to be performed
  io.cmp_out := MuxLookup(fn, Bool(false), Seq(
    FN_EQ -> (in1_xor_in2 === UInt(0)),
    FN_NEQ -> (in1_xor_in2 != UInt(0)),
    FN_SLT -> in1_lt_in2,
    FN_SLTU -> in1_ltu_in2,
    FN_SGE -> (in1_lt_in2 | (in1_xor_in2 === UInt(0))),
    FN_SGEU -> (in1_ltu_in2 | (in1_xor_in2 === UInt(0))) ))

  // For SLT & SLTU, we also need to write to a register value
  val slt_out = UInt(width=xLen)
  slt_out := Mux(fn === FN_SLT || fn === FN_SLTU, io.cmp_out, UInt(0))

  // Shifter component - used for the following instructions
  // SLL, SRL, SRA, SLLI, SRLI, SRAI
  // SLLW, SRLW, SRAW, SLLIW, SRLIW, SRAIW
  val sh_amt = UInt(width=6)
  when (io.inst.op === UInt(0x3B) |
        io.inst.op === UInt(0x1B)) {
    // SLLW, SRLW, SRAW, SLLIW, SRLIW, SRAIW
    // Only consider the low 5 bits for deciding shift amount
    sh_amt := Cat(Bits(0, width=1), in2(4,0))
  } .otherwise {
    // SLL, SRL, SRA, SLLI, SRLI, SRAI
    // They consider the low 6 bits for deciding the shift amount
    sh_amt := in2(5,0)
  }
  val sh_out_l = (in1 << sh_amt)(xLen-1,0)
  // Sign bits are extended only for SRA
  val sign_bit = in1(xLen-1)
  val sh_out_r = (Cat(fn === FN_SRA & sign_bit,
                  in1(xLen-1,0)).asSInt >> sh_amt)(xLen-1,0)
  val sh_out = Mux(fn === FN_SRL || fn === FN_SRA, sh_out_r, UInt(0)) |
               Mux(fn === FN_SLL, sh_out_l, UInt(0))
  val final_sh_out = UInt(width=xLen)

  when (io.inst.op === UInt(0x3B) |
        io.inst.op === UInt(0x1B)) {
    // SLLW, SRLW, SRAW, SLLIW, SRLIW, SRAIW instructions - Need to consider
    // only 32 bits of the result and sign-extend it
    final_sh_out := Cat(Fill(32, sh_out(31)), sh_out(31,0))
  } .otherwise {
    final_sh_out := sh_out
  }

  // AND, OR, XOR
  val logic = MuxLookup(fn, UInt(0), Seq(
    FN_XOR -> in1_xor_in2,
    FN_OR -> UInt(in1(xLen-1,0) | in2(xLen-1,0)),
    FN_AND -> UInt(in1(xLen-1,0) & in2(xLen-1,0))))

  val non_arith_out = slt_out | logic | final_sh_out
  val isBranch = io.inst.op === UInt(0x63)
  io.out := Mux(isBranch, branch_addr_out,
            Mux(fn === FN_ADD || fn === FN_SUB, final_adder_out, non_arith_out))
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
    "LBU" -> (0x3, 0x4, 0x0),
    "LBH" -> (0x3, 0x5, 0x0),

    "SB" -> (0x23, 0x0, 0x0),
    "SH" -> (0x23, 0x1, 0x0),
    "SW" -> (0x23, 0x2, 0x0),

    "BEQ" -> (0x63, 0x0, 0x0),
    "BNE" -> (0x63, 0x1, 0x0),
    "BLT" -> (0x63, 0x4, 0x0),
    "BGE" -> (0x63, 0x5, 0x0),
    "BLTU" -> (0x63, 0x6, 0x0),
    "BGEU" -> (0x63, 0x7, 0x0),

    "JAL" -> (0x6F, 0x0, 0x0),
    "JALR" -> (0x67, 0x0, 0x0),

    "LUI" -> (0x37, 0x0, 0x0),
    "AUIPC" -> (0x17, 0x0, 0x0),

    "LD" -> (0x3, 0x3, 0x0),
    "LWU" -> (0x3, 0x6, 0x0),

    "SD" -> (0x23, 0x3, 0x0),

    "ADDW" -> (0x3B, 0x0, 0x0),
    "SUBW" -> (0x3B, 0x0, 0x20),
    "SLLW" -> (0x3B, 0x1, 0x0),
    "SRLW" -> (0x3B, 0x5, 0x0),
    "SRAW" -> (0x3B, 0x5, 0x20),

    "ADDIW" -> (0x1B, 0x0, 0x0),
    "SLLIW" -> (0x1B, 0x1, 0x0),
    "SRLIW" -> (0x1B, 0x5, 0x0),
    "SRAIW" -> (0x1B, 0x5, 0x20)
  )
  // Utility function for setting opcode, funct3 and funct7 values
  def set_instruction(inst_name : String) = {
    println("Testing " + inst_name + " instruction")
    val value = inst_map(inst_name)
    poke(c.io.inst.op, value._1)
    poke(c.io.inst.funct3, value._2)
    poke(c.io.inst.funct7, value._3)
  }

  // 1. Test ADD instruction - Sanity and larger than 32 bit values
  set_instruction("ADD")
  poke(c.io.rs1_val, 0x0ffffffffffffffeL)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 0x0fffffffffffffffL)

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

  // 4. Test SUB instruction - Sanity and larger than 32-bit values
  set_instruction("SUB")
  poke(c.io.rs1_val, 0x0fffffffffffffffL)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 0x0ffffffffffffffeL)

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
  // The low 6 bits are all zero for rs2_val. We expect the result to be equal
  // to rs1_val
  poke(c.io.rs2_val, 64)
  peek(c.sh_amt)
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

  // 19. Test SRL instruction - Verify that only the 6 low bits of rs2_val are
  // considered.
  set_instruction("SRL")
  poke(c.io.rs1_val, 50)
  // The low 6 bits are all zero for rs2_val. We expect the result to be equal to
  // rs1_val
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

  // 23. Test SRA instruction - Verify that only the 6 low bits of rs2_val are
  // considered.
  set_instruction("SRA")
  poke(c.io.rs1_val, 50)
  // The low 6 bits are all zero for rs2_val. We expect the result to be equal to
  // rs1_val
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

  // 30. Test SLLI instruction - Verify that only the low 6 bits of IMMI are
  // considered
  set_instruction("SLLI")
  poke(c.io.rs1_val, 100)
  // The low 6 bits are all zero for IMMI. We expect the result to be equal to
  // rs1_val
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

  // 41. Test SRLI instruction - Verify that only the low 6 bits of IMMI are
  // considered.
  set_instruction("SRLI")
  poke(c.io.rs1_val, 50)
  // The low 6 bits are all zero for IMMI. We expect the result to be equal to
  // rs1_val
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

  // 45. Test SRAI instruction - Verify that only the low 6 bits of IMMI are
  // considered.
  set_instruction("SRAI")
  poke(c.io.rs1_val, 50)
  // The low 6 bits are all zero for IMMI. We expect the result to be equal to
  // rs1_val
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

  // 65. Test BEQ instruction - Equal values, Positive IMMB value
  set_instruction("BEQ")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 66. Test BEQ instruction - Unequal values, Positive IMMB value
  set_instruction("BEQ")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 51)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 67. Test BEQ instruction - Equal values, Negative IMMB value
  set_instruction("BEQ")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 68. Test BEQ instruction - Unequal values, Negative IMMB value
  set_instruction("BEQ")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 51)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 69. Test BNE instruction - Equal values, Positive IMMB value
  set_instruction("BNE")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 70. Test BNE instruction - Unequal values, Positive IMMB value
  set_instruction("BNE")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 51)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 71. Test BNE instruction - Equal values, Negative IMMB value
  set_instruction("BNE")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 72. Test BNE instruction - Unequal values, Negative IMMB value
  set_instruction("BNE")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 51)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 73. Test BLT instruction - Positive less than, Positive IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 74. Test BLT instruction - Positive not less than, Positive IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 75. Test BLT instruction - Negative less than, Positive IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 76. Test BLT instruction - Negative not less than, Positive IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -2L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 77. Test BLT instruction - Positive less than, Negative IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 78. Test BLT instruction - Positive not less than, Negative IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 79. Test BLT instruction - Negative less than, Negative IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 80. Test BLT instruction - Negative not less than, Negative IMMB value
  set_instruction("BLT")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 81. Test BLTU instruction - Positive less than, Positive IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 82. Test BLTU instruction - Positive not less than, Positive IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 83. Test BLTU instruction - Negative less than, Positive IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  // BLTU ignores the sign bit, hence we get the opposite result
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 84. Test BLTU instruction - Negative not less than, Positive IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  // BLTU ignores the sign bit, hence we get the opposite result
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 85. Test BLTU instruction - Positive less than, Negative IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 86. Test BLTU instruction - Positive not less than, Negative IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 87. Test BLTU instruction - Negative less than, Negative IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  // BLTU ignores the sign bit, hence we get the opposite result
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 88. Test BLTU instruction - Negative not less than, Negative IMMB value
  set_instruction("BLTU")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  // BLTU ignores the sign bit, hence we get the opposite result
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 89. Test BGE instruction - Positive greater, Positive IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 90. Test BGE instruction - Positive equal, Positive IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 91. Test BGE instruction - Positive less than, Positive IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 92. Test BGE instruction - Negative greater, Positive IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 93. Test BGE instruction - Negative equal, Positive IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 94. Test BGE instruction - Negative less than, Positive IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 95. Test BGE instruction - Positive greater, Negative IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 96. Test BGE instruction - Positive equal, Negative IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 97. Test BGE instruction - Positive less than, Negative IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 98. Test BGE instruction - Negative greater, Negative IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -2L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 99. Test BGE instruction - Negative equal, Negative IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 100. Test BGE instruction - Negative less than, Negative IMMB value
  set_instruction("BGE")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 101. Test BGEU instruction - Positive greater, Positive IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 102. Test BGEU instruction - Positive equal, Positive IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 103. Test BGE instruction - Positive less than, Positive IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 104. Test BGE instruction - Negative greater, Positive IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  // BGEU ignores the sign bit, hence we get the opposite result
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 1100)

  // 105. Test BGEU instruction - Negative equal, Positive IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 106. Test BGEU instruction - Negative less than, Positive IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, 100)
  step(1)
  // BGEU ignores the sign bit, hence we get the opposite result
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 1100)

  // 107. Test BGEU instruction - Positive greater, Negative IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 108. Test BGEU instruction - Positive equal, Negative IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 109. Test BGEU instruction - Positive less than, Negative IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 0)
  expect(c.io.out, 900)

  // 110. Test BGEU instruction - Negative greater, Negative IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -2L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 111. Test BGEU instruction - Negative equal, Negative IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -1L)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 112. Test BGEU instruction - Negative less than, Negative IMMB value
  set_instruction("BGEU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 100)
  poke(c.io.PC, 1000)
  poke(c.io.inst.immB, -100)
  step(1)
  // BGEU ignores the sign bit, hence we get the opposite result
  expect(c.io.cmp_out, 1)
  expect(c.io.out, 900)

  // 113. Test JAL instruction - Positive IMMJ value
  set_instruction("JAL")
  poke(c.io.PC, 1000)
  poke(c.io.inst.immJ, 100)
  step(1)
  expect(c.io.out, 1104)

  // 114. Test JAL instruction - Negative IMMJ value
  set_instruction("JAL")
  poke(c.io.PC, 1000)
  poke(c.io.inst.immJ, -100)
  step(1)
  expect(c.io.out, 904)

  // 115. Test JALR instruction - Positive IMMI value
  set_instruction("JALR")
  poke(c.io.PC, 1000)
  poke(c.io.inst.immI, 100)
  step(1)
  expect(c.io.out, 1104)

  // 116. Test LUI instruction
  set_instruction("LUI")
  poke(c.io.inst.immU, 10)
  step(1)
  expect(c.io.out, 10 << 12)

  // 117. Test AUIPC instruction
  set_instruction("AUIPC")
  poke(c.io.PC, 1000)
  poke(c.io.inst.immU, 10)
  step(1)
  expect(c.io.out, 1000 + (10 << 12))

  // 118. Test LD instruction - Positive IMMI value
  set_instruction("LD")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 119. Test LD instruction - Negative IMMI value
  set_instruction("LD")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 120. Test LWU instruction - Positive IMMI value
  set_instruction("LWU")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 121. Test LWU instruction - Negative IMMI value
  set_instruction("LWU")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, -50)
  step(1)
  expect(c.io.out, 50)

  // 122. Test SD instruction - Positive IMMS value
  set_instruction("SD")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immS, 50)
  step(1)
  expect(c.io.out, 100)

  // 123. Test SD instruction - Negative IMMS value
  set_instruction("SD")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immS, -50)
  step(1)
  expect(c.io.out, 50)

  // 124. Test SLLW instruction - sanity
  set_instruction("SLLW")
  poke(c.io.rs1_val, 100)
  poke(c.io.rs2_val, 2)
  step(1)
  expect(c.io.out, 400)

  // 125. Test SLLW instruction - Verify that only the 5 lower bits in rs2_val are
  // taken. 
  set_instruction("SLLW")
  poke(c.io.rs1_val, 1)
  // The low 5 bits are all zero for rs2_val. We expect the result to be equal
  // to rs1_val
  poke(c.io.rs2_val, 32)
  peek(c.sh_amt)
  step(1)
  expect(c.io.out, 1)

  // 126. Test SLLW instruction - Verify that only the low 32 bits of inputs
  // are considered.
  set_instruction("SLLW")
  poke(c.io.rs1_val, 0x800000000L)
  poke(c.io.rs2_val, 0)
  step(1)
  expect(c.io.out, 0)

  // 127. Test SLLW instruction - Verify that only the low 32 bits of output
  // are generated
  set_instruction("SLLW")
  poke(c.io.rs1_val, 0x80000000)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 0)

  // 128. Test SLLW instruction - Verify that only the low 32 bits are
  // generated with the correct sign extension
  set_instruction("SLLW")
  poke(c.io.rs1_val, 0xc0000000)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 0xffffffff80000000L)

  // 129. Test SRLW instruction - sanity
  set_instruction("SRLW")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 50 >> 1)

  // 130. Test SRLW instruction - Verify that only the 5 low bits of rs2_val are
  // considered.
  set_instruction("SRLW")
  poke(c.io.rs1_val, 50)
  // The low 5 bits are all zero for rs2_val. We expect the result to be equal to
  // rs1_val
  poke(c.io.rs2_val, 32)
  step(1)
  expect(c.io.out, 50)

  // 131. Test SRLW instruction - Verify that only the low 32 bits of inputs
  // are considered.
  set_instruction("SRLW")
  poke(c.io.rs1_val, 0x800000000L)
  poke(c.io.rs2_val, 0)
  step(1)
  expect(c.io.out, 0)

  // 132. Test SRAW instruction - Sanity
  set_instruction("SRAW")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 50 >> 1)

  // 133. Test SRAW instruction - Verify that sign extended bit is not ignored
  set_instruction("SRAW")
  poke(c.io.rs1_val, 0xfffffffc)
  poke(c.io.rs2_val, 2)
  step(1)
  // XXX-kbavishi: What is the SRAW instruction supposed to do here?
  // Is it supposed to consider the sign bit of the 32-bit value?
  expect(c.io.out, 0xffffffffffffffffL)

  // 134. Test SRAW instruction - Verify that only the 5 low bits of rs2_val are
  // considered.
  set_instruction("SRAW")
  poke(c.io.rs1_val, 50)
  // The low 5 bits are all zero for rs2_val. We expect the result to be equal to
  // rs1_val
  poke(c.io.rs2_val, 32)
  step(1)
  expect(c.io.out, 50)

  // 135. Test SRAW instruction - Verify that only the low 32 bits of inputs
  // are considered.
  set_instruction("SRAW")
  poke(c.io.rs1_val, 0x800000000L)
  poke(c.io.rs2_val, 0)
  step(1)
  expect(c.io.out, 0)

  // 136. Test SLLIW instruction - Sanity
  set_instruction("SLLIW")
  poke(c.io.rs1_val, 100)
  poke(c.io.inst.immI, 2)
  step(1)
  expect(c.io.out, 400)

  // 137. Test SLLIW instruction - Verify that only the low 5 bits of IMMI are
  // considered
  set_instruction("SLLIW")
  poke(c.io.rs1_val, 100)
  // The low 5 bits are all zero for IMMI. We expect the result to be equal to
  // rs1_val
  poke(c.io.inst.immI, 32)
  step(1)
  expect(c.io.out, 100)

  // 138. Test SLLIW instruction - Verify that only the low 32 bits of inputs
  // are considered.
  set_instruction("SLLIW")
  poke(c.io.rs1_val, 0x800000000L)
  poke(c.io.inst.immI, 0)
  step(1)
  expect(c.io.out, 0)

  // 139. Test SLLIW instruction - Verify that only the low 32 bits of output
  // are generated
  set_instruction("SLLIW")
  poke(c.io.rs1_val, 0x80000000)
  poke(c.io.inst.immI, 1)
  step(1)
  expect(c.io.out, 0)

  // 140. Test SLLIW instruction - Verify that only the low 32 bits are
  // generated with the correct sign extension
  set_instruction("SLLIW")
  poke(c.io.rs1_val, 0xc0000000)
  poke(c.io.inst.immI, 1)
  step(1)
  expect(c.io.out, 0xffffffff80000000L)

  // 141. Test SRLIW instruction - sanity
  set_instruction("SRLIW")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 1)
  step(1)
  expect(c.io.out, 25)

  // 142. Test SRLIW instruction - Verify that only the low 5 bits of IMMI are
  // considered.
  set_instruction("SRLIW")
  poke(c.io.rs1_val, 50)
  // The low 5 bits are all zero for IMMI. We expect the result to be equal to
  // rs1_val
  poke(c.io.inst.immI, 32)
  step(1)
  expect(c.io.out, 50)

  // 143. Test SRAIW instruction - Sanity
  set_instruction("SRAIW")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 1)
  step(1)
  expect(c.io.out, 25)

  // 144. Test SRAIW instruction - Verify that sign extended bit is not ignored
  set_instruction("SRAIW")
  poke(c.io.rs1_val, 0xfffffffc)
  poke(c.io.inst.immI, 2)
  step(1)
  // XXX-kbavishi: What is the SRAIW instruction supposed to do here?
  // Is it supposed to consider the sign bit of the 32-bit value?
  expect(c.io.out, 0xffffffffffffffffL)

  // 145. Test SRAIW instruction - Verify that only the low 5 bits of IMMI are
  // considered.
  set_instruction("SRAIW")
  poke(c.io.rs1_val, 50)
  // The low 5 bits are all zero for IMMI. We expect the result to be equal to
  // rs1_val
  poke(c.io.inst.immI, 32)
  step(1)
  expect(c.io.out, 50)

  // 146. Test SRAIW instruction - Verify that only the low 32 bits of inputs
  // are considered.
  set_instruction("SRAIW")
  poke(c.io.rs1_val, 0x800000000L)
  poke(c.io.inst.immI, 0)
  step(1)
  expect(c.io.out, 0)

  // 147. Test ADDW instruction - Sanity
  set_instruction("ADDW")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 100)

  // 148. Test ADDW instruction - one 32-bit negative value and sign extension
  // of lower 32 bits
  set_instruction("ADDW")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, -100)
  step(1)
  expect(c.io.out, -50L)

  // 149. Test ADDW instruction - two 32-bit negative values and sign extension
  set_instruction("ADDW")
  poke(c.io.rs1_val, -100)
  poke(c.io.rs2_val, -100)
  step(1)
  expect(c.io.out, -200L)

  // 150. Test ADDW instruction - Verify that only the low 32 bits of inputs
  // are considered
  set_instruction("ADDW")
  poke(c.io.rs1_val, 0x800000001L)
  poke(c.io.rs2_val, 0x800000001L)
  step(1)
  expect(c.io.out, 0x2)

  // 151. Test ADDW instruction - Also verify that only the low 32 bits of output
  // generated
  set_instruction("ADDW")
  poke(c.io.rs1_val, 0xffffffff)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 0)

  // 152. Test SUBW instruction - Sanity
  set_instruction("SUBW")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 0)

  // 153. Test SUBW instruction - Sanity 2 and sign extension
  set_instruction("SUBW")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  step(1)
  expect(c.io.out, -50L)

  // 154. Test SUBW instruction - one 32-bit negative value and sign extension
  set_instruction("SUBW")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, -50)
  step(1)
  expect(c.io.out, 100)

  // 155. Test SUBW instruction - two 32-bit negative values
  set_instruction("SUBW")
  poke(c.io.rs1_val, -50)
  poke(c.io.rs2_val, -60)
  step(1)
  expect(c.io.out, 10L)

  // 156. Test SUBW instruction - Verify that only the low 32 bits of inputs
  // are considered
  set_instruction("SUBW")
  poke(c.io.rs1_val, 0x8000000002L)
  poke(c.io.rs2_val, 0x7000000001L)
  step(1)
  expect(c.io.out, 0x1)

  // 157. Test SUBW instruction - Verify that only the low 32 bits of result
  // are considered and correctly sign extended
  set_instruction("SUBW")
  poke(c.io.rs1_val, 0xffffffff)
  poke(c.io.rs2_val, -1)
  step(1)
  expect(c.io.out, 0)

  // 158. Test ADDIW instruction - Sanity
  set_instruction("ADDIW")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, 50)
  step(1)
  expect(c.io.out, 100)

  // 159. Test ADDIW instruction - one 32-bit negative value and sign extension
  // of lower 32 bits
  set_instruction("ADDIW")
  poke(c.io.rs1_val, 50)
  poke(c.io.inst.immI, -100)
  step(1)
  expect(c.io.out, -50L)

  // 160. Test ADDIW instruction - two 32-bit negative values and sign extension
  set_instruction("ADDIW")
  poke(c.io.rs1_val, -100)
  poke(c.io.inst.immI, -100)
  step(1)
  expect(c.io.out, -200L)

  // 161. Test ADDIW instruction - Verify that only the low 32 bits of inputs
  // are considered
  set_instruction("ADDIW")
  poke(c.io.rs1_val, 0x800000001L)
  poke(c.io.inst.immI, 0x1)
  step(1)
  expect(c.io.out, 0x2)

  // 162. Test ADDIW instruction - Also verify that only the low 32 bits of output
  // generated
  set_instruction("ADDIW")
  poke(c.io.rs1_val, 0xffffffff)
  poke(c.io.inst.immI, 1)
  step(1)
  expect(c.io.out, 0)

  // 163. Test LUI instruction - Sign-extension testcase
  set_instruction("LUI")
  poke(c.io.inst.immU, 0x80000)
  step(1)
  expect(c.io.out, 0xffffffff80000000L)

  // 164. Test AUIPC instruction - Sign-extension testcase
  set_instruction("AUIPC")
  poke(c.io.PC, 1000)
  poke(c.io.inst.immU, 0x80000)
  step(1)
  expect(c.io.out, 0xffffffff800003e8L)

}

class ALUGenerator extends TestGenerator {
  def genMod(): Module = Module(new ALU(64))
  def genTest[T <: Module](c: T): Tester[T] =
    (new ALUTests(c.asInstanceOf[ALU])).asInstanceOf[Tester[T]]
}

