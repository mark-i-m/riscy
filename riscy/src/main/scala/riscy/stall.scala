package riscy

import Chisel._

class RiscyStall extends Module {
  val io = new Bundle {
    // TODO
  }
}

class RiscyStallTests(c: RiscyStall) extends Tester(c) {
  println("TODO")
}

class StallGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyStall())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyStallTests(c.asInstanceOf[RiscyStall])).asInstanceOf[Tester[T]]
}
