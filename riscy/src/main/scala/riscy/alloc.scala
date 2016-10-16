package riscy

import Chisel._

class RiscyAlloc extends Module {

}

class RiscyAllocTests(c: RiscyAlloc) extends Tester(c) {

}

class AllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new RiscyAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new RiscyAllocTests(c.asInstanceOf[RiscyAlloc])).asInstanceOf[Tester[T]]
}
