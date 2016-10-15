
package rocket

import Chisel._

class RiscyDecode extends Module {
  val io = new Bundle {
    // Input from I$
    val fourIns = UInt(INPUT, 128);

    // Decoded output signals
    val op = UInt(OUTPUT, 7)
    val rs1 = UInt(OUTPUT, 5)
    val rs2 = UInt(OUTPUT, 5)
    val rd = UInt(OUTPUT, 5)
    val funct3 = UInt(OUTPUT, 3)
    val funct7 = UInt(OUTPUT, 7)
    val imm = UInt(OUTPUT, 25)

    // Should we stall?
    val stall = Bool(INPUT)
  }




}
