package riscy

import Chisel._
import scala.language.reflectiveCalls

class DCacheStReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = UInt(INPUT, 64)
  val size = UInt(INPUT, 3)
}

class DCacheLdReq extends Bundle {
  val addr = Valid(UInt(INPUT, 64)).asInput
  val data = Valid(UInt(OUTPUT, 64)).asOutput
}

class DCache extends Module {
  val io = new Bundle {
    val stReq = Vec(2, new DCacheStReq)
    val ldReq = new DCacheLdReq
    val memStAddrPort = Vec(2, Valid(UInt(OUTPUT,64).asOutput))
    val memStData = Vec(2, UInt(OUTPUT,64))
    val memStSize = Vec(2, UInt(OUTPUT,3))
    val memLdAddrPort = Valid(UInt(OUTPUT,64)).asOutput
    val memLdData = Valid(UInt(INPUT,8 * 64)).asInput
  }

  for (i <- 0 until 2) {
    io.memStAddrPort(i).valid := io.stReq(i).addr.valid
    io.memStAddrPort(i).bits := io.stReq(i).addr.bits
    io.memStData(i) := io.stReq(i).data
    /*
     * size = 0x0 -> Store low byte
     * size = 0x1 -> Store low half-word
     * size = 0x2 -> Store low word
     * size = 0x3 -> Store entire double word
     */
    io.memStSize(i) := io.stReq(i).size
  }

  io.memLdAddrPort.valid := io.ldReq.addr.valid
  io.memLdAddrPort.bits := io.ldReq.addr.bits
  io.ldReq.data.bits := io.memLdData.bits(63,0)
  io.ldReq.data.valid := io.memLdData.valid
}

class DCacheTests(c: DCache) extends Tester(c) {
  println("TODO")
}

class DCacheGenerator extends TestGenerator {
  def genMod(): Module = Module(new DCache())
  def genTest[T <: Module](c: T): Tester[T] =
    (new DCacheTests(c.asInstanceOf[DCache])).asInstanceOf[Tester[T]]
}
