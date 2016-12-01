package riscy

import Chisel._

class Execute extends Module {
  val io = new Bundle {
    // Instructions to execute
    val issued_inst = Vec(4, Valid(new ROBEntry).asInput)

    // Stored values for 2 cycles which can be used for bypass
    val rob_wb_store = new RobWbStore(6) // OUTPUT

    // These values are to be written back to the ROB
    val rob_wb_output = new RobWbOutput(6) // OUTPUT

    // Values coming in from LSQ for WB structure
    val rob_wb_input = new RobWbInput(2)

    // TODO markm: maybe add these back if we do fancy mispeculation scheme
    // Indicates whether the branch was taken. Valid only if the instruction
    // executed by the ALU was a branch. If valid, the `bits` attribute will
    // indicate whether the branch was taken.
    val is_branch_taken = Vec(4, Valid(Bool(OUTPUT)))

    // The computed branch targets. Use in conjunction with is_branch_taken ie.
    // only if the valid bit is set.
    val branch_target = Vec(4, UInt(OUTPUT, 64))

    // The tags for the branch instructions. Use only if valid bit is set for
    // is_branch_taken
    val branch_tag = Vec(4, UInt(OUTPUT, 6))

    // The PCs of the branch instructions. Use only if valid bit is set for
    // is_branch_taken
    val branch_PC = Vec(4, UInt(OUTPUT, 64))
  }

  val alu = Array.fill(4)(Module(new ALU(64)))
  val rob_writeback = Module(new RobWriteback(6))

  // Hook up ALUs with issued instructions
  for(i <- 0 until 4) {
    alu(i).io.PC          := io.issued_inst(i).bits.pc
    alu(i).io.rs1_val     := io.issued_inst(i).bits.rs1Val.bits
    alu(i).io.rs2_val     := io.issued_inst(i).bits.rs2Val.bits
    // Hook up only parts of DecodeIns we really need
    alu(i).io.inst        := io.issued_inst(i).bits
    alu(i).io.inst.funct3 := io.issued_inst(i).bits.funct3
    alu(i).io.inst.funct7 := io.issued_inst(i).bits.funct7
    alu(i).io.inst.immI   := io.issued_inst(i).bits.immI
    alu(i).io.inst.immS   := io.issued_inst(i).bits.immS
    alu(i).io.inst.immB   := io.issued_inst(i).bits.immB
    alu(i).io.inst.immU   := io.issued_inst(i).bits.immU
    alu(i).io.inst.immJ   := io.issued_inst(i).bits.immJ
  }

  // Hook up the output of ALUs to ROB writeback structure
  for(i <- 0 until 4) {
    rob_writeback.io.input.data(i)    := alu(i).io.out
    rob_writeback.io.input.is_addr(i) := alu(i).io.is_out_addr
    rob_writeback.io.input.operand(i) := io.issued_inst(i).bits.tag
    rob_writeback.io.input.valid(i)   := io.issued_inst(i).valid
  }

  // Hook up the output of LSQ to ROB writeback structure
  for(i <- 0 until 2) {
    rob_writeback.io.input.data(3+i)    := io.rob_wb_input.data(i)     
    rob_writeback.io.input.is_addr(3+i) := io.rob_wb_input.is_addr(i) 
    rob_writeback.io.input.operand(3+i) := io.rob_wb_input.operand(i) 
    rob_writeback.io.input.valid(3+i)   := io.rob_wb_input.valid(i)   
  }	

  // Hook up certain output attributes of ALUs to the outside world
  for(i <- 0 until 4) {
    io.is_branch_taken(i).valid := alu(i).io.is_branch
    io.is_branch_taken(i).bits  := alu(i).io.cmp_out
    io.branch_target(i)         := alu(i).io.out
    io.branch_tag(i)            := io.issued_inst(i).bits.tag
    io.branch_PC(i)             := io.issued_inst(i).bits.pc
  }

  // Hookup the output of ROB writeback structure to the outside world
  io.rob_wb_store  <> rob_writeback.io.store
  io.rob_wb_output <> rob_writeback.io.output
}

class ExecuteTests(c: Execute) extends Tester(c) {
  // XXX-kbavishi: Maybe move this to a common place?
  // Used for populating opcodes, funct3 and funct7 values. Format is:
  // "INST_NAME" -> (opcode, funct3, funct7)
  val inst_map = Map(
    "ADD" -> (0x33, 0x0, 0x0),
    "ADDI" -> (0x13, 0x0, 0x0),
    "BEQ" -> (0x63, 0x0, 0x0),
    "BNE" -> (0x63, 0x1, 0x0),
    "BLT" -> (0x63, 0x4, 0x0)
  )
  // Utility function for setting opcode, funct3 and funct7 values
  def set_instruction(index : Int, inst_name : String) = {
    println("Testing " + inst_name + " instruction")
    val value = inst_map(inst_name)
    poke(c.io.issued_inst(index).valid, true)
    poke(c.io.issued_inst(index).bits.op, value._1)
    poke(c.io.issued_inst(index).bits.funct3, value._2)
    poke(c.io.issued_inst(index).bits.funct7, value._3)
  }

  // 0. Add valid 4 instructions
  set_instruction(0, "ADD")
  poke(c.io.issued_inst(0).bits.tag, 1)
  poke(c.io.issued_inst(0).bits.rs1Val.bits, 100)
  poke(c.io.issued_inst(0).bits.rs2Val.bits, 150)

  set_instruction(1, "ADDI")
  poke(c.io.issued_inst(1).bits.tag, 2)
  poke(c.io.issued_inst(1).bits.rs1Val.bits, 100)
  poke(c.io.issued_inst(1).bits.immI, 150)

  set_instruction(2, "BEQ")
  poke(c.io.issued_inst(2).bits.tag, 3)
  poke(c.io.issued_inst(2).bits.rs1Val.bits, 100)
  poke(c.io.issued_inst(2).bits.rs2Val.bits, 150)
  poke(c.io.issued_inst(2).bits.pc, 10000)
  poke(c.io.issued_inst(2).bits.immB, 400)

  set_instruction(3, "BNE")
  poke(c.io.issued_inst(3).bits.tag, 4)
  poke(c.io.issued_inst(3).bits.rs1Val.bits, 100)
  poke(c.io.issued_inst(3).bits.rs2Val.bits, 150)
  poke(c.io.issued_inst(3).bits.pc, 10000)
  poke(c.io.issued_inst(3).bits.immB, 400)
  step(1)

  // 1. Check results in ROB WB store and check whether branch targets are
  // correctly computed

  // Check ADD result
  expect(c.io.rob_wb_store.valid_s1(0), true)
  expect(c.io.rob_wb_store.data_s1(0), 250)
  expect(c.io.rob_wb_store.is_addr_s1(0), false)
  expect(c.io.rob_wb_store.operand_s1(0), 1)
  expect(c.io.is_branch_taken(0).valid, false)

  // Check ADDI result
  expect(c.io.rob_wb_store.valid_s1(1), true)
  expect(c.io.rob_wb_store.data_s1(1), 250)
  expect(c.io.rob_wb_store.is_addr_s1(1), false)
  expect(c.io.rob_wb_store.operand_s1(1), 2)
  expect(c.io.is_branch_taken(1).valid, false)

  // Check BEQ result
  expect(c.io.rob_wb_store.valid_s1(2), true)
  expect(c.io.rob_wb_store.data_s1(2), 10400)
  expect(c.io.rob_wb_store.is_addr_s1(2), true)
  expect(c.io.rob_wb_store.operand_s1(2), 3)
  expect(c.io.is_branch_taken(2).valid, true)
  expect(c.io.is_branch_taken(2).bits, false)
  expect(c.io.branch_tag(2), 3)
  expect(c.io.branch_PC(2), 10000)

  // Check BNE result
  expect(c.io.rob_wb_store.valid_s1(3), true)
  expect(c.io.rob_wb_store.data_s1(3), 10400)
  expect(c.io.rob_wb_store.is_addr_s1(3), true)
  expect(c.io.rob_wb_store.operand_s1(3), 4)
  expect(c.io.is_branch_taken(3).valid, true)
  expect(c.io.is_branch_taken(3).bits, true)
  expect(c.io.branch_tag(3), 4)
  expect(c.io.branch_PC(3), 10000)

  // Add 2 valid instructions and 2 invalid instructions
  set_instruction(0, "ADD")
  poke(c.io.issued_inst(0).bits.tag, 5)
  poke(c.io.issued_inst(0).bits.rs1Val.bits, -1L)
  poke(c.io.issued_inst(0).bits.rs2Val.bits, -2L)

  set_instruction(1, "BLT")
  poke(c.io.issued_inst(1).bits.tag, 6)
  poke(c.io.issued_inst(1).bits.rs1Val.bits, 100)
  poke(c.io.issued_inst(1).bits.rs2Val.bits, 200)
  poke(c.io.issued_inst(1).bits.pc, 20000)
  poke(c.io.issued_inst(1).bits.immB, 200)

  poke(c.io.issued_inst(2).valid, false)
  poke(c.io.issued_inst(3).valid, false)
  step(1)

  // 2. Check results in ROB WB store and check whether branch targets are
  // correctly computed. Only 2 ALU results should be valid.

  // Check ADD result
  expect(c.io.rob_wb_store.valid_s1(0), true)
  expect(c.io.rob_wb_store.data_s1(0), -3L)
  expect(c.io.rob_wb_store.is_addr_s1(0), false)
  expect(c.io.rob_wb_store.operand_s1(0), 5)
  expect(c.io.is_branch_taken(0).valid, false)

  // Check BLT result
  expect(c.io.rob_wb_store.valid_s1(1), true)
  expect(c.io.rob_wb_store.data_s1(1), 20200)
  expect(c.io.rob_wb_store.is_addr_s1(1), true)
  expect(c.io.rob_wb_store.operand_s1(1), 6)
  expect(c.io.is_branch_taken(1).valid, true)
  expect(c.io.is_branch_taken(1).bits, true)
  expect(c.io.branch_tag(1), 6)
  expect(c.io.branch_PC(1), 20000)

  // Check invalid instructions
  expect(c.io.rob_wb_store.valid_s1(2), false)
  expect(c.io.rob_wb_store.valid_s1(2), false)

  // Also verify that results computed 2 cycles ago are now output from ROB WB.
  // Check ADD result
  expect(c.io.rob_wb_output.valid(0), true)
  expect(c.io.rob_wb_output.data(0), 250)
  expect(c.io.rob_wb_output.is_addr(0), false)
  expect(c.io.rob_wb_output.operand(0), 1)

  // Check ADDI result
  expect(c.io.rob_wb_output.valid(1), true)
  expect(c.io.rob_wb_output.data(1), 250)
  expect(c.io.rob_wb_output.is_addr(1), false)
  expect(c.io.rob_wb_output.operand(1), 2)

  // Check BEQ result
  expect(c.io.rob_wb_output.valid(2), true)
  expect(c.io.rob_wb_output.data(2), 10400)
  expect(c.io.rob_wb_output.is_addr(2), true)
  expect(c.io.rob_wb_output.operand(2), 3)

  // Check BNE result
  expect(c.io.rob_wb_output.valid(3), true)
  expect(c.io.rob_wb_output.data(3), 10400)
  expect(c.io.rob_wb_output.is_addr(3), true)
  expect(c.io.rob_wb_output.operand(3), 4)

}

class ExecuteGenerator extends TestGenerator {
  def genMod(): Module = Module(new Execute)
  def genTest[T <: Module](c: T): Tester[T] =
    (new ExecuteTests(c.asInstanceOf[Execute])).asInstanceOf[Tester[T]]
}
