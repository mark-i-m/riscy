package riscy

import Chisel._

class LSQEntry extends Bundle {
  // TODO
}

class LSQ extends Module {
  val io = new Bundle {
    // TODO
  }

  val addrqW = Vec.fill(4) { Valid(new LSQEntry) }
  val addrq = Vec.tabulate(4) { i => RegEnable(addrqW(i).bits, addrqW(i).valid) }
}

class LSQTests(c: LSQ) extends Tester(c) {
  println("TODO")
}

class LSQGenerator extends TestGenerator {
  def genMod(): Module = Module(new LSQ())
  def genTest[T <: Module](c: T): Tester[T] =
    (new LSQTests(c.asInstanceOf[LSQ])).asInstanceOf[Tester[T]]
}
