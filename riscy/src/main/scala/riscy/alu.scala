package riscy

import Chisel._

class ALU extends Module {
  val io = new Bundle {
    val op = Valid(UInt(INPUT, 7))
    val func3 = UInt(INPUT, 3)
    val func7 = UInt(INPUT, 7)

    val in1 = UInt(INPUT, 64)
    val in2 = UInt(INPUT, 64)

    // For branches, indicate if the branch was predicted taken
    val prediction = Bool(INPUT)

    val result = UInt(OUTPUT, 64)

    // For branches, all indicate if the branch was mispredicted
    val mispredicted = Bool(OUTPUT)
  }

  // TODO
}
