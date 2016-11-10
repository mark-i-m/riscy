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
  } .otherwise {
    op := FN_UNKNOWN
  }

  // Selector logic for deciding which inputs must be chosen for the ALU to
  // perform the operation
  val sel_a1 = UInt(width=2)
  val sel_a2 = UInt(width=2)
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
  } .otherwise {
    sel_a1 := A1_RS1
    sel_a2 := A2_RS2
  }

  // Select the inputs for ALU operations
  val in1 = MuxLookup(sel_a1, UInt(0), Seq(
    A1_RS1 -> io.rs1_val,
    A1_PC -> io.PC))
  val in2 = MuxLookup(sel_a2, UInt(0), Seq(
    A2_RS2 -> io.rs2_val,
    A2_IMM_I -> io.inst.immI.asUInt))

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
  // Drop the sign bit if we are doing SRL
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
    "AND" -> (0x33, 0x7, 0x0)
  )
  // Utility function for setting opcode, funct3 and funct7 values
  def set_instruction(inst_name : String) = {
    println("Testing " + inst_name + " instruction")
    val value = inst_map(inst_name)
    poke(c.io.inst.op, value._1)
    poke(c.io.inst.funct3, value._2)
    poke(c.io.inst.funct7, value._3)
  }

  // Test ADD instruction
  set_instruction("ADD")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 100)

  // Test SUB instruction
  set_instruction("SUB")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 0)

  // Test SLL instruction - sanity
  set_instruction("SLL")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 4)
  step(1)
  expect(c.io.out, 50 << 4)

  // Test SLL instruction - Verify that only the 5 lower bits in rs2_val are
  // taken. 
  set_instruction("SLL")
  poke(c.io.rs1_val, 1)
  // The lower five bits are all zero for rs2_val. We expect the result to be
  // equal to rs1_val
  poke(c.io.rs2_val, 64)
  step(1)
  expect(c.io.out, 1)

  // Test SLT instruction - sanity 1 (not less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 0)
  
  // Test SLT instruction - sanity 2 (less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  step(1)
  expect(c.io.out, 1)

  // Test SLT instruction - Signed comparison sanity 1 (not less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, -2L)
  step(1)
  expect(c.io.out, 0)

  // Test SLT instruction - Signed comparison sanity 2 (less than)
  set_instruction("SLT")
  poke(c.io.rs1_val, -2L)
  poke(c.io.rs2_val, -1L)
  step(1)
  expect(c.io.out, 1)

  // Test SLTU instruction - sanity 1 (not less than)
  set_instruction("SLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 50)
  step(1)
  expect(c.io.out, 0)
  
  // Test SLT instruction - sanity 2 (less than)
  set_instruction("SLTU")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 100)
  step(1)
  expect(c.io.out, 1)

  // Test SLT instruction - sanity 3 (Pick one huge number. Should not be
  // considered as a signed value)
  set_instruction("SLTU")
  poke(c.io.rs1_val, -1L)
  poke(c.io.rs2_val, 0)
  step(1)
  expect(c.io.out, 0)

  // Test XOR instruction 
  set_instruction("XOR")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.rs2_val, 234567890)
  step(1)
  expect(c.io.out, 123456789 ^ 234567890)

  // Test SRL instruction - sanity
  set_instruction("SRL")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 50 >> 1)

  // Test SRL instruction - Verify that only 5 lower bits of rs2_val are
  // considered.
  set_instruction("SRL")
  poke(c.io.rs1_val, 50)
  // The lower five bits are all zero for rs2_val. We expect the result to be
  // equal to rs1_val
  poke(c.io.rs2_val, 64)
  step(1)
  expect(c.io.out, 50)

  // Test SRL instruction - Verify that sign extended bit is ignored
  set_instruction("SRL")
  // Set to -4 ie. 0xfffffffffffffffc
  poke(c.io.rs1_val, -4L)
  poke(c.io.rs2_val, 2)
  step(1)
  // Since SRL ignores the sign bit and just brings in zeroes, we expect it to
  // convert to 0x3fffffffffffffff
  expect(c.io.out, 0x3fffffffffffffffL)

  // Test SRA instruction - Sanity
  set_instruction("SRA")
  poke(c.io.rs1_val, 50)
  poke(c.io.rs2_val, 1)
  step(1)
  expect(c.io.out, 50 >> 1)

  // Test SRA instruction - Verify that sign extended bit is not ignored
  set_instruction("SRA")
  poke(c.io.rs1_val, -4L)
  poke(c.io.rs2_val, 2)
  step(1)
  expect(c.io.out, -1L)

  // Test SRA instruction - Verify that only 5 lower bits of rs2_val are
  // considered.
  set_instruction("SRA")
  poke(c.io.rs1_val, 50)
  // The lower five bits are all zero for rs2_val. We expect the result to be
  // equal to rs1_val
  poke(c.io.rs2_val, 64)
  step(1)
  expect(c.io.out, 50)

  // Test OR instruction 
  set_instruction("OR")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.rs2_val, 234567890)
  step(1)
  expect(c.io.out, 123456789 | 234567890)

  // Test AND instruction 
  set_instruction("AND")
  poke(c.io.rs1_val, 123456789)
  poke(c.io.rs2_val, 234567890)
  step(1)
  expect(c.io.out, 123456789 & 234567890)
}

class ALUGenerator extends TestGenerator {
  def genMod(): Module = Module(new ALU(64))
  def genTest[T <: Module](c: T): Tester[T] =
    (new ALUTests(c.asInstanceOf[ALU])).asInstanceOf[Tester[T]]
}

