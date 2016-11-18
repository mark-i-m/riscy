package riscy

import Chisel._
import scala.language.reflectiveCalls

class LSQEntry extends AddBufEntry {
  // TODO
  val addr = UInt(OUTPUT, 32)
  val value = UInt(OUTPUT, 64)
  val ready  = Bool(OUTPUT) // Entry populated or not
}

class LSQ extends Module {
  val io = new Bundle {
    // TODO
    val addr = UInt(INPUT, 32)
    val value = UInt(INPUT, 64)
    val robLoc = UInt(INPUT, 6)
    val stCommit = Bool(INPUT)
    val resEntry = Vec.fill(4) { Valid(new AddBufEntry).flip }
    val fillEntry = Valid(new LSQEntry)
    val issuedEntry = Valid(new LSQEntry).asOutput
    val currentLen = UInt(OUTPUT, 4)
    val stAddr = Valid(UInt(OUTPUT, 32))
    val stValue = UInt(OUTPUT, 64)
  }

  // Number of entries in the ROB
  val DEPTH = 32

  //val addrqW = Vec.fill(4) { Valid(new LSQEntry) }
  val addrqW = Vec.fill(DEPTH) { Valid(new LSQEntry) }
  val addrq = Vec.tabulate(DEPTH) { i => RegEnable(addrqW(i).bits, addrqW(i).valid) }

  //val counter = new CounterUpDown(DEPTH)

  val resVector: UInt = Cat(io.resEntry(3).valid, io.resEntry(2).valid, io.resEntry(1).valid, io.resEntry(0).valid)
  // Number of entries being reserved in the queue
  val resNum = PopCount(resVector)

  // Length of the queue
  val queueLength = UInt()

  // Register holding pointer to top of the queue
  val qTop = Reg(init = UInt(0), next = queueLength + resNum)
  queueLength := qTop
  
  // Wires that will feed inputs to the queue
  val res = Vec.fill(4) { Valid(new AddBufEntry).flip }

  // Logic to determine routing of inputs
  // FIXME this is buggy
  when (resVector === UInt(15)) {
    res(0) := io.resEntry(0)
    res(1) := io.resEntry(1)
    res(2) := io.resEntry(2)
    res(3) := io.resEntry(3)
  } .elsewhen (resVector === UInt(14)) {
    res(0) := io.resEntry(1)
    res(1) := io.resEntry(2)
    res(2) := io.resEntry(3)
    res(3).valid := Bool(false)
  } .elsewhen (resVector === UInt(13)) {
    res(0) := io.resEntry(0)
    res(3).valid := Bool(false)
    res(1) := io.resEntry(2)
    res(2) := io.resEntry(3)
  } .elsewhen (resVector === UInt(12)) {
    res(2).valid := Bool(false)
    res(3).valid := Bool(false)
    res(0) := io.resEntry(2)
    res(1) := io.resEntry(3)
  } .elsewhen (resVector === UInt(11)) {
    res(0) := io.resEntry(0)
    res(1) := io.resEntry(1)
    res(3).valid := Bool(false)
    res(2) := io.resEntry(3)
  } .elsewhen (resVector === UInt(10)) {
    res(2).valid := Bool(false)
    res(0) := io.resEntry(1)
    res(3).valid := Bool(false)
    res(1) := io.resEntry(3)
  } .elsewhen (resVector === UInt(9)) {
    res(0) := io.resEntry(0)
    res(2).valid := Bool(false)
    res(3).valid := Bool(false)
    res(1) := io.resEntry(3)
  } .elsewhen (resVector === UInt(8)) {
    res(3).valid := Bool(false)
    res(1).valid := Bool(false)
    res(2).valid := Bool(false)
    res(0) := io.resEntry(3)
  } .elsewhen (resVector === UInt(7)) {
    res(0) := io.resEntry(0)
    res(1) := io.resEntry(1)
    res(2) := io.resEntry(2)
    res(3).valid := Bool(false)
  } .elsewhen (resVector === UInt(6)) {
    res(2).valid := Bool(false)
    res(0) := io.resEntry(1)
    res(1) := io.resEntry(2)
    res(3).valid := Bool(false)
  } .elsewhen (resVector === UInt(5)) {
    res(0) := io.resEntry(0)
    res(2).valid := Bool(false)
    res(1) := io.resEntry(2)
    res(3).valid := Bool(false)
  } .elsewhen (resVector === UInt(4)) {
    res(2).valid := Bool(false)
    res(1).valid := Bool(false)
    res(0) := io.resEntry(2)
    res(3).valid := Bool(false)
  } .elsewhen (resVector === UInt(3)) {
    res(0) := io.resEntry(0)
    res(1) := io.resEntry(1)
    res(2).valid := Bool(false)
    res(3).valid := Bool(false)
  } .elsewhen (resVector === UInt(2)) {
    res(1).valid := Bool(false)
    res(0) := io.resEntry(1)
    res(2).valid := Bool(false)
    res(3).valid := Bool(false)
  } .elsewhen (resVector === UInt(1)) {
    res(0) := io.resEntry(0)
    res(1).valid := Bool(false)
    res(2).valid := Bool(false)
    res(3).valid := Bool(false)
  } .otherwise {
    res(0).valid := Bool(false)
    res(1).valid := Bool(false)
    res(2).valid := Bool(false)
    res(3).valid := Bool(false)
  }

  // Hook up inputs to entries in the queue
  for (i <- 0 until 4) {
    addrq(queueLength + UInt(i)) := res(i)
  }

  // Feedback to the arbiter on the number of entries filled in the queue
  io.currentLen := queueLength

  val depRow = Array.tabulate(DEPTH) { i => UInt(i) -> Reg(UInt(1)) }
  val depMatrix = Array.tabulate(DEPTH) { i => UInt(i) -> depRow }

  for (i <- 0 until DEPTH) {
    depMatrix(i)._2(i)._2 := UInt(0)
  }

  val tagMatch = Vec.fill(DEPTH) { Bool() }
  val addrMatch = Vec.fill(DEPTH) { Bool() }
  for (i <- 0 until DEPTH) {
    tagMatch(i) := (io.robLoc === addrq(i).robLoc)
    when (tagMatch(i)) {
      addrq(i).addr := io.addr
    }
  }

  // The dependency matrix
  for (i <- 0 until DEPTH) {
    for (j <- 0 until DEPTH) {
      addrMatch(j) := (io.addr === addrq(j).addr)
        when (addrMatch(j)) {
          //addrq(i).value := io.value
          depMatrix(i)._2(i)._2 := UInt(1)
        }
    }
  }

  io.stAddr.bits := addrq(0).addr
  io.stValue := addrq(0).ready
  when (io.stCommit) {
    io.stAddr.valid := Bool(true)
  } .otherwise {
    io.stAddr.valid := Bool(false)
  }

}

class LSQTests(c: LSQ) extends Tester(c) {
  println("TODO")
}

class LSQGenerator extends TestGenerator {
  def genMod(): Module = Module(new LSQ())
  def genTest[T <: Module](c: T): Tester[T] =
    (new LSQTests(c.asInstanceOf[LSQ])).asInstanceOf[Tester[T]]
}
