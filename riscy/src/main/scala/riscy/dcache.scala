package riscy

import Chisel._
import scala.language.reflectiveCalls

class DCacheStReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = UInt(INPUT, 64)
}

class DCacheLdReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = UInt(OUTPUT, 64)
}

class DCache extends Module {
  val io = new Bundle {
    val stReq = Vec(2, new DCacheStReq)
    val ldReq = new DCacheLdReq
    val memStAddrPort = Vec(2, Valid(UInt(OUTPUT,64).asOutput))
    val memStData = Vec(2, UInt(OUTPUT,64))
    val memLdAddrPort = Valid(UInt(OUTPUT,64)).asOutput
    val memLdData = Valid(UInt(INPUT,8 * 64)).asInput
  }

  io.memStAddrPort(0).valid := io.stReq(0).addr.valid
  io.memStAddrPort(0).bits := io.stReq(0).addr.bits
  io.memStAddrPort(1).valid := io.stReq(1).addr.valid
  io.memStAddrPort(1).bits := io.stReq(1).addr.bits
  io.memStData(0) := io.stReq(0).data
  io.memStData(1) := io.stReq(1).data

  io.memLdAddrPort.valid := io.ldReq.addr.valid
  io.memLdAddrPort.bits := io.ldReq.addr.bits
  io.ldReq.data := io.memLdData.bits(63,0)
}

class DCacheTests(c: DCache) extends Tester(c) {
  println("TODO")
}

class DCacheGenerator extends TestGenerator {
  def genMod(): Module = Module(new DCache())
  def genTest[T <: Module](c: T): Tester[T] =
    (new DCacheTests(c.asInstanceOf[DCache])).asInstanceOf[Tester[T]]
}
