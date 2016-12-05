package riscy

import Chisel._

class Execute extends Module {
  val io = new Bundle {
    // Instructions to execute
    val issued_inst = Vec(4, Valid(new ROBEntry).asInput)

    // Speculation info related to bypassing, given by IQ
    val specIssue = Vec(4, Valid(new SpeculativeIssue).asInput)

    // Stored values for 2 cycles which can be used for bypass
    val rob_wb_store = new RobWbStore(6) // OUTPUT

    // These values are to be written back to the ROB
    val rob_wb_output = new RobWbOutput(6) // OUTPUT

    // Values coming in from LSQ for WB structure
    val rob_wb_input = new RobWbInput(2)
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

    // Hook up speculative info related to bypassing
    alu(i).io.specIssue   := io.specIssue(i)
    alu(i).io.rob_wb_store <> rob_writeback.io.store
  }

  // Hook up the output of ALUs to ROB writeback structure
  for(i <- 0 until 4) {
    rob_writeback.io.input.entry(i).data                  := alu(i).io.out
    rob_writeback.io.input.entry(i).is_addr               := alu(i).io.is_out_addr
    rob_writeback.io.input.entry(i).operand               := io.issued_inst(i).bits.tag
    rob_writeback.io.input.entry(i).valid                 := io.issued_inst(i).valid
    rob_writeback.io.input.entry(i).is_branch_taken.valid := alu(i).io.is_branch
    rob_writeback.io.input.entry(i).is_branch_taken.bits  := alu(i).io.cmp_out
    rob_writeback.io.input.entry(i).branch_target         := alu(i).io.out
    rob_writeback.io.input.entry(i).branch_tag            := io.issued_inst(i).bits.tag
    rob_writeback.io.input.entry(i).branch_PC             := io.issued_inst(i).bits.pc
  }

  // Hook up the output of LSQ to ROB writeback structure
  for(i <- 0 until 2) {
    rob_writeback.io.input.entry(4+i).data    := io.rob_wb_input.entry(i).data
    rob_writeback.io.input.entry(4+i).is_addr := io.rob_wb_input.entry(i).is_addr
    rob_writeback.io.input.entry(4+i).operand := io.rob_wb_input.entry(i).operand
    rob_writeback.io.input.entry(4+i).valid   := io.rob_wb_input.entry(i).valid
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
  expect(c.io.rob_wb_store.entry_s1(0).valid, true)
  expect(c.io.rob_wb_store.entry_s1(0).data, 250)
  expect(c.io.rob_wb_store.entry_s1(0).is_addr, false)
  expect(c.io.rob_wb_store.entry_s1(0).operand, 1)
  expect(c.io.rob_wb_store.entry_s1(0).is_branch_taken.valid, false)

  // Check ADDI result
  expect(c.io.rob_wb_store.entry_s1(1).valid, true)
  expect(c.io.rob_wb_store.entry_s1(1).data, 250)
  expect(c.io.rob_wb_store.entry_s1(1).is_addr, false)
  expect(c.io.rob_wb_store.entry_s1(1).operand, 2)
  expect(c.io.rob_wb_store.entry_s1(1).is_branch_taken.valid, false)

  // Check BEQ result
  expect(c.io.rob_wb_store.entry_s1(2).valid, true)
  expect(c.io.rob_wb_store.entry_s1(2).data, 10400)
  expect(c.io.rob_wb_store.entry_s1(2).is_addr, true)
  expect(c.io.rob_wb_store.entry_s1(2).operand, 3)
  expect(c.io.rob_wb_store.entry_s1(2).is_branch_taken.valid, true)
  expect(c.io.rob_wb_store.entry_s1(2).is_branch_taken.bits, false)
  expect(c.io.rob_wb_store.entry_s1(2).branch_tag, 3)
  expect(c.io.rob_wb_store.entry_s1(2).branch_PC, 10000)

  // Check BNE result
  expect(c.io.rob_wb_store.entry_s1(3).valid, true)
  expect(c.io.rob_wb_store.entry_s1(3).data, 10400)
  expect(c.io.rob_wb_store.entry_s1(3).is_addr, true)
  expect(c.io.rob_wb_store.entry_s1(3).operand, 4)
  expect(c.io.rob_wb_store.entry_s1(3).is_branch_taken.valid, true)
  expect(c.io.rob_wb_store.entry_s1(3).is_branch_taken.bits, true)
  expect(c.io.rob_wb_store.entry_s1(3).branch_tag, 4)
  expect(c.io.rob_wb_store.entry_s1(3).branch_PC, 10000)

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
  expect(c.io.rob_wb_store.entry_s1(0).valid, true)
  expect(c.io.rob_wb_store.entry_s1(0).data, -3L)
  expect(c.io.rob_wb_store.entry_s1(0).is_addr, false)
  expect(c.io.rob_wb_store.entry_s1(0).operand, 5)
  expect(c.io.rob_wb_store.entry_s1(0).is_branch_taken.valid, false)

  // Check BLT result
  expect(c.io.rob_wb_store.entry_s1(1).valid, true)
  expect(c.io.rob_wb_store.entry_s1(1).data, 20200)
  expect(c.io.rob_wb_store.entry_s1(1).is_addr, true)
  expect(c.io.rob_wb_store.entry_s1(1).operand, 6)
  expect(c.io.rob_wb_store.entry_s1(1).is_branch_taken.valid, true)
  expect(c.io.rob_wb_store.entry_s1(1).is_branch_taken.bits, true)
  expect(c.io.rob_wb_store.entry_s1(1).branch_tag, 6)
  expect(c.io.rob_wb_store.entry_s1(1).branch_PC, 20000)

  // Check invalid instructions
  expect(c.io.rob_wb_store.entry_s1(2).valid, false)
  expect(c.io.rob_wb_store.entry_s1(2).valid, false)

  // Also verify that results computed 2 cycles ago are now output from ROB WB.
  // Check ADD result
  expect(c.io.rob_wb_output.entry(0).valid, true)
  expect(c.io.rob_wb_output.entry(0).data, 250)
  expect(c.io.rob_wb_output.entry(0).is_addr, false)
  expect(c.io.rob_wb_output.entry(0).operand, 1)

  // Check ADDI result
  expect(c.io.rob_wb_output.entry(1).valid, true)
  expect(c.io.rob_wb_output.entry(1).data, 250)
  expect(c.io.rob_wb_output.entry(1).is_addr, false)
  expect(c.io.rob_wb_output.entry(1).operand, 2)

  // Check BEQ result
  expect(c.io.rob_wb_output.entry(2).valid, true)
  expect(c.io.rob_wb_output.entry(2).data, 10400)
  expect(c.io.rob_wb_output.entry(2).is_addr, true)
  expect(c.io.rob_wb_output.entry(2).operand, 3)

  // Check BNE result
  expect(c.io.rob_wb_output.entry(3).valid, true)
  expect(c.io.rob_wb_output.entry(3).data, 10400)
  expect(c.io.rob_wb_output.entry(3).is_addr, true)
  expect(c.io.rob_wb_output.entry(3).operand, 4)

}

class ExecuteGenerator extends TestGenerator {
  def genMod(): Module = Module(new Execute)
  def genTest[T <: Module](c: T): Tester[T] =
    (new ExecuteTests(c.asInstanceOf[Execute])).asInstanceOf[Tester[T]]
}
