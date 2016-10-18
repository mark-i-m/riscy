package riscy

import Chisel._

// In CSR registers Imm value is taken as CSR reg number so we have to decide how to handle it - TODO
// System instructions have same opcode as CSR instructions, but they are not I-type and they are not taken care at this stage. Only difference between CSR and System is Funct3 (0 for system calls)- TODO 
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
}

class RiscyOpDecode extends Module {
  val io = new Bundle {
    // Input opcode (7 bits) from RiscyDecode module
    val op = UInt(INPUT, 7)
    // Output to RiscyAlloc for ROB entry generation
    val opInfo = new OpDecode()
}  

if (io.op(6,2) === 0x0C  || io.op(6,2) === 0x0E) { 										// R type - ADDW,SUBW,SLLW,SRLW,SRAW,ADD,SUB,SLL,SLT,SLTU,XOR,SRL,SRA,OR,AND
  io.opInfo.hasRs1    = 1
  io.opInfo.hasRs2    = 1
  io.opInfo.hasRd     = 1
  io.opInfo.hasFunct3 = 1
  io.opInfo.hasFunct7 = 1
  io.opInfo.hasImmI   = 0
  io.opInfo.hasImmS   = 0
  io.opInfo.hasImmB   = 0
  io.opInfo.hasImmU   = 0
  io.opInfo.hasImmJ   = 0
} else if (io.op(6,2) === 0x04 || io.op(6,2) === 0x00 || io.op(6,2) === 0x19 || io.op(6,2) === 0x06 || io.op(6,2) === 0x1C) { 	// I type - JALR,ADDI,SLLI,SLTI,SLTIU,XORI,SRLI,SRAI,ORI,ANDI,ADDIW,SLLIW,SRLIW,SRAIW,LB,LH,LW,LD,LBU,LHU,LWU,CSRRW,CSRRS,CSRRC,CSRRWI,CSRRSI,CSRRCI
  io.opInfo.hasRs1    = 1
  io.opInfo.hasRs2    = 0
  io.opInfo.hasRd     = 1
  io.opInfo.hasFunct3 = 1
  io.opInfo.hasFunct7 = 1
  io.opInfo.hasImmI   = 1
  io.opInfo.hasImmS   = 0
  io.opInfo.hasImmB   = 0
  io.opInfo.hasImmU   = 0
  io.opInfo.hasImmJ   = 0
} else if (io.op(6,2) === 0x08 ) { 												// S type - SB,SH,SW,SD 
  io.opInfo.hasRs1    = 1
  io.opInfo.hasRs2    = 1
  io.opInfo.hasRd     = 0 
  io.opInfo.hasFunct3 = 1
  io.opInfo.hasFunct7 = 0
  io.opInfo.hasImmI   = 0
  io.opInfo.hasImmS   = 1
  io.opInfo.hasImmB   = 0
  io.opInfo.hasImmU   = 0
  io.opInfo.hasImmJ   = 0
} else if (io.op(6,2) === 0x18) {			 									// SB type - BEQ,BNE,BLT,BGE,BLTE,BGUE
  io.opInfo.hasRs1    = 1
  io.opInfo.hasRs2    = 1
  io.opInfo.hasRd     = 0 
  io.opInfo.hasFunct3 = 1
  io.opInfo.hasFunct7 = 0
  io.opInfo.hasImmI   = 0
  io.opInfo.hasImmS   = 0
  io.opInfo.hasImmB   = 1
  io.opInfo.hasImmU   = 0
  io.opInfo.hasImmJ   = 0
} else if (io.op(6,2) === 0x0D || io.op(6,2) === 0x05) { 									// U Type - LUI,AUIPC 
  io.opInfo.hasRs1    = 0
  io.opInfo.hasRs2    = 0 
  io.opInfo.hasRd     = 1
  io.opInfo.hasFunct3 = 0
  io.opInfo.hasFunct7 = 0
  io.opInfo.hasImmI   = 0
  io.opInfo.hasImmS   = 0
  io.opInfo.hasImmB   = 0
  io.opInfo.hasImmU   = 1
  io.opInfo.hasImmJ   = 0
} else if (io.op(6,2) === 0x1B) { 												// UJ Type - JAL 
  io.opInfo.hasRs1    = 0
  io.opInfo.hasRs2    = 0 
  io.opInfo.hasRd     = 1
  io.opInfo.hasFunct3 = 0
  io.opInfo.hasFunct7 = 0
  io.opInfo.hasImmI   = 0
  io.opInfo.hasImmS   = 0
  io.opInfo.hasImmB   = 0
  io.opInfo.hasImmU   = 0
  io.opInfo.hasImmJ   = 1
}

class RiscyOpDecodeTests(c: RiscyOpDecode) extends Tester(c) {
  println("TODO")
}

class OpDecodeGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyOpDecode())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyOpDecodeTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
} 
