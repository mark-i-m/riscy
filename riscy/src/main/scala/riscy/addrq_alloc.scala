package riscy

import Chisel._
import scala.language.reflectiveCalls

class AddrQueueAlloc extends Module {
  val io = new Bundle {
    val validEntries = Vec(32, Bool(INPUT))
    val pos = Vec(4, UInt(OUTPUT, width=5))
  }
  
  io.pos(0) := PriorityEncoder(Vec.tabulate(32) { i => !io.validEntries(i) })

  for (i <- 1 until 4) {
    io.pos(i) := UInt(i,width=5)
  }

  switch (io.pos(0)) {
    is (UInt(1)) {
      io.pos(1) := UInt(2) + PriorityEncoder(Vec.tabulate(32-1-1) { i => !io.validEntries(i+1+1) })
    }
    is (UInt(2)) {
      io.pos(1) := UInt(3) + PriorityEncoder(Vec.tabulate(32-2-1) { i => !io.validEntries(i+2+1) })
    }
    is (UInt(3)) {
      io.pos(1) := UInt(4) + PriorityEncoder(Vec.tabulate(32-3-1) { i => !io.validEntries(i+3+1) })
    }
    is (UInt(4)) {
      io.pos(1) := UInt(5) + PriorityEncoder(Vec.tabulate(32-4-1) { i => !io.validEntries(i+4+1) })
    }
    is (UInt(5)) {
      io.pos(1) := UInt(6) + PriorityEncoder(Vec.tabulate(32-5-1) { i => !io.validEntries(i+5+1) })
    }
    is (UInt(6)) {
      io.pos(1) := UInt(7) + PriorityEncoder(Vec.tabulate(32-6-1) { i => !io.validEntries(i+6+1) })
    }
    is (UInt(7)) {
      io.pos(1) := UInt(8) + PriorityEncoder(Vec.tabulate(32-7-1) { i => !io.validEntries(i+7+1) })
    }
    is (UInt(8)) {
      io.pos(1) := UInt(9) + PriorityEncoder(Vec.tabulate(32-8-1) { i => !io.validEntries(i+8+1) })
    }
    is (UInt(9)) {
      io.pos(1) := UInt(10) + PriorityEncoder(Vec.tabulate(32-9-1) { i => !io.validEntries(i+9+1) })
    }
    is (UInt(10)) {
      io.pos(1) := UInt(11) + PriorityEncoder(Vec.tabulate(32-10-1) { i => !io.validEntries(i+10+1) })
    }
    is (UInt(11)) {
      io.pos(1) := UInt(12) + PriorityEncoder(Vec.tabulate(32-11-1) { i => !io.validEntries(i+11+1) })
    }
    is (UInt(12)) {
      io.pos(1) := UInt(13) + PriorityEncoder(Vec.tabulate(32-12-1) { i => !io.validEntries(i+12+1) })
    }
    is (UInt(13)) {
      io.pos(1) := UInt(14) + PriorityEncoder(Vec.tabulate(32-13-1) { i => !io.validEntries(i+13+1) })
    }
    is (UInt(14)) {
      io.pos(1) := UInt(15) + PriorityEncoder(Vec.tabulate(32-14-1) { i => !io.validEntries(i+14+1) })
    }
    is (UInt(15)) {
      io.pos(1) := UInt(16) + PriorityEncoder(Vec.tabulate(32-15-1) { i => !io.validEntries(i+15+1) })
    }
    is (UInt(16)) {
      io.pos(1) := UInt(17) + PriorityEncoder(Vec.tabulate(32-16-1) { i => !io.validEntries(i+16+1) })
    }
    is (UInt(17)) {
      io.pos(1) := UInt(18) + PriorityEncoder(Vec.tabulate(32-17-1) { i => !io.validEntries(i+17+1) })
    }
    is (UInt(18)) {
      io.pos(1) := UInt(19) + PriorityEncoder(Vec.tabulate(32-18-1) { i => !io.validEntries(i+18+1) })
    }
    is (UInt(19)) {
      io.pos(1) := UInt(20) + PriorityEncoder(Vec.tabulate(32-19-1) { i => !io.validEntries(i+19+1) })
    }
    is (UInt(20)) {
      io.pos(1) := UInt(21) + PriorityEncoder(Vec.tabulate(32-20-1) { i => !io.validEntries(i+20+1) })
    }
    is (UInt(21)) {
      io.pos(1) := UInt(22) + PriorityEncoder(Vec.tabulate(32-21-1) { i => !io.validEntries(i+21+1) })
    }
    is (UInt(22)) {
      io.pos(1) := UInt(23) + PriorityEncoder(Vec.tabulate(32-22-1) { i => !io.validEntries(i+22+1) })
    }
    is (UInt(23)) {
      io.pos(1) := UInt(24) + PriorityEncoder(Vec.tabulate(32-23-1) { i => !io.validEntries(i+23+1) })
    }
    is (UInt(24)) {
      io.pos(1) := UInt(25) + PriorityEncoder(Vec.tabulate(32-24-1) { i => !io.validEntries(i+24+1) })
    }
    is (UInt(25)) {
      io.pos(1) := UInt(26) + PriorityEncoder(Vec.tabulate(32-25-1) { i => !io.validEntries(i+25+1) })
    }
    is (UInt(26)) {
      io.pos(1) := UInt(27) + PriorityEncoder(Vec.tabulate(32-26-1) { i => !io.validEntries(i+26+1) })
    }
    is (UInt(27)) {
      io.pos(1) := UInt(28) + PriorityEncoder(Vec.tabulate(32-27-1) { i => !io.validEntries(i+27+1) })
    }
    is (UInt(28)) {
      io.pos(1) := UInt(29) + PriorityEncoder(Vec.tabulate(32-28-1) { i => !io.validEntries(i+28+1) })
    }
    is (UInt(29)) {
      io.pos(1) := UInt(30) + PriorityEncoder(Vec.tabulate(32-29-1) { i => !io.validEntries(i+29+1) })
    }
    is (UInt(30)) {
      io.pos(1) := UInt(31) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !io.validEntries(i+30+1) })
    }
    is (UInt(31)) {
      io.pos(1) := UInt(0) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !io.validEntries(i+30+1) })
    }
    is (UInt(0)) {
      io.pos(1) := UInt(1) + PriorityEncoder(Vec.tabulate(32-0-1) { i => !io.validEntries(i+0+1) })
    }
  }

  switch (io.pos(1)) {
    is (UInt(1)) {
      io.pos(2) := UInt(2) + PriorityEncoder(Vec.tabulate(32-1-1) { i => !io.validEntries(i+1+1) })
    }
    is (UInt(2)) {
      io.pos(2) := UInt(3) + PriorityEncoder(Vec.tabulate(32-2-1) { i => !io.validEntries(i+2+1) })
    }
    is (UInt(3)) {
      io.pos(2) := UInt(4) + PriorityEncoder(Vec.tabulate(32-3-1) { i => !io.validEntries(i+3+1) })
    }
    is (UInt(4)) {
      io.pos(2) := UInt(5) + PriorityEncoder(Vec.tabulate(32-4-1) { i => !io.validEntries(i+4+1) })
    }
    is (UInt(5)) {
      io.pos(2) := UInt(6) + PriorityEncoder(Vec.tabulate(32-5-1) { i => !io.validEntries(i+5+1) })
    }
    is (UInt(6)) {
      io.pos(2) := UInt(7) + PriorityEncoder(Vec.tabulate(32-6-1) { i => !io.validEntries(i+6+1) })
    }
    is (UInt(7)) {
      io.pos(2) := UInt(8) + PriorityEncoder(Vec.tabulate(32-7-1) { i => !io.validEntries(i+7+1) })
    }
    is (UInt(8)) {
      io.pos(2) := UInt(9) + PriorityEncoder(Vec.tabulate(32-8-1) { i => !io.validEntries(i+8+1) })
    }
    is (UInt(9)) {
      io.pos(2) := UInt(10) + PriorityEncoder(Vec.tabulate(32-9-1) { i => !io.validEntries(i+9+1) })
    }
    is (UInt(10)) {
      io.pos(2) := UInt(11) + PriorityEncoder(Vec.tabulate(32-10-1) { i => !io.validEntries(i+10+1) })
    }
    is (UInt(11)) {
      io.pos(2) := UInt(12) + PriorityEncoder(Vec.tabulate(32-11-1) { i => !io.validEntries(i+11+1) })
    }
    is (UInt(12)) {
      io.pos(2) := UInt(13) + PriorityEncoder(Vec.tabulate(32-12-1) { i => !io.validEntries(i+12+1) })
    }
    is (UInt(13)) {
      io.pos(2) := UInt(14) + PriorityEncoder(Vec.tabulate(32-13-1) { i => !io.validEntries(i+13+1) })
    }
    is (UInt(14)) {
      io.pos(2) := UInt(15) + PriorityEncoder(Vec.tabulate(32-14-1) { i => !io.validEntries(i+14+1) })
    }
    is (UInt(15)) {
      io.pos(2) := UInt(16) + PriorityEncoder(Vec.tabulate(32-15-1) { i => !io.validEntries(i+15+1) })
    }
    is (UInt(16)) {
      io.pos(2) := UInt(17) + PriorityEncoder(Vec.tabulate(32-16-1) { i => !io.validEntries(i+16+1) })
    }
    is (UInt(17)) {
      io.pos(2) := UInt(18) + PriorityEncoder(Vec.tabulate(32-17-1) { i => !io.validEntries(i+17+1) })
    }
    is (UInt(18)) {
      io.pos(2) := UInt(19) + PriorityEncoder(Vec.tabulate(32-18-1) { i => !io.validEntries(i+18+1) })
    }
    is (UInt(19)) {
      io.pos(2) := UInt(20) + PriorityEncoder(Vec.tabulate(32-19-1) { i => !io.validEntries(i+19+1) })
    }
    is (UInt(20)) {
      io.pos(2) := UInt(21) + PriorityEncoder(Vec.tabulate(32-20-1) { i => !io.validEntries(i+20+1) })
    }
    is (UInt(21)) {
      io.pos(2) := UInt(22) + PriorityEncoder(Vec.tabulate(32-21-1) { i => !io.validEntries(i+21+1) })
    }
    is (UInt(22)) {
      io.pos(2) := UInt(23) + PriorityEncoder(Vec.tabulate(32-22-1) { i => !io.validEntries(i+22+1) })
    }
    is (UInt(23)) {
      io.pos(2) := UInt(24) + PriorityEncoder(Vec.tabulate(32-23-1) { i => !io.validEntries(i+23+1) })
    }
    is (UInt(24)) {
      io.pos(2) := UInt(25) + PriorityEncoder(Vec.tabulate(32-24-1) { i => !io.validEntries(i+24+1) })
    }
    is (UInt(25)) {
      io.pos(2) := UInt(26) + PriorityEncoder(Vec.tabulate(32-25-1) { i => !io.validEntries(i+25+1) })
    }
    is (UInt(26)) {
      io.pos(2) := UInt(27) + PriorityEncoder(Vec.tabulate(32-26-1) { i => !io.validEntries(i+26+1) })
    }
    is (UInt(27)) {
      io.pos(2) := UInt(28) + PriorityEncoder(Vec.tabulate(32-27-1) { i => !io.validEntries(i+27+1) })
    }
    is (UInt(28)) {
      io.pos(2) := UInt(29) + PriorityEncoder(Vec.tabulate(32-28-1) { i => !io.validEntries(i+28+1) })
    }
    is (UInt(29)) {
      io.pos(2) := UInt(30) + PriorityEncoder(Vec.tabulate(32-29-1) { i => !io.validEntries(i+29+1) })
    }
    is (UInt(30)) {
      io.pos(2) := UInt(31) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !io.validEntries(i+30+1) })
    }
    is (UInt(31)) {
      io.pos(2) := UInt(0) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !io.validEntries(i+30+1) })
    }
    is (UInt(0)) {
      io.pos(2) := UInt(1) + PriorityEncoder(Vec.tabulate(32-0-1) { i => !io.validEntries(i+0+1) })
    }
  }
  switch (io.pos(2)) {
    is (UInt(1)) {
      io.pos(3) := UInt(2) + PriorityEncoder(Vec.tabulate(32-1-1) { i => !io.validEntries(i+1+1) })
    }
    is (UInt(2)) {
      io.pos(3) := UInt(3) + PriorityEncoder(Vec.tabulate(32-2-1) { i => !io.validEntries(i+2+1) })
    }
    is (UInt(3)) {
      io.pos(3) := UInt(4) + PriorityEncoder(Vec.tabulate(32-3-1) { i => !io.validEntries(i+3+1) })
    }
    is (UInt(4)) {
      io.pos(3) := UInt(5) + PriorityEncoder(Vec.tabulate(32-4-1) { i => !io.validEntries(i+4+1) })
    }
    is (UInt(5)) {
      io.pos(3) := UInt(6) + PriorityEncoder(Vec.tabulate(32-5-1) { i => !io.validEntries(i+5+1) })
    }
    is (UInt(6)) {
      io.pos(3) := UInt(7) + PriorityEncoder(Vec.tabulate(32-6-1) { i => !io.validEntries(i+6+1) })
    }
    is (UInt(7)) {
      io.pos(3) := UInt(8) + PriorityEncoder(Vec.tabulate(32-7-1) { i => !io.validEntries(i+7+1) })
    }
    is (UInt(8)) {
      io.pos(3) := UInt(9) + PriorityEncoder(Vec.tabulate(32-8-1) { i => !io.validEntries(i+8+1) })
    }
    is (UInt(9)) {
      io.pos(3) := UInt(10) + PriorityEncoder(Vec.tabulate(32-9-1) { i => !io.validEntries(i+9+1) })
    }
    is (UInt(10)) {
      io.pos(3) := UInt(11) + PriorityEncoder(Vec.tabulate(32-10-1) { i => !io.validEntries(i+10+1) })
    }
    is (UInt(11)) {
      io.pos(3) := UInt(12) + PriorityEncoder(Vec.tabulate(32-11-1) { i => !io.validEntries(i+11+1) })
    }
    is (UInt(12)) {
      io.pos(3) := UInt(13) + PriorityEncoder(Vec.tabulate(32-12-1) { i => !io.validEntries(i+12+1) })
    }
    is (UInt(13)) {
      io.pos(3) := UInt(14) + PriorityEncoder(Vec.tabulate(32-13-1) { i => !io.validEntries(i+13+1) })
    }
    is (UInt(14)) {
      io.pos(3) := UInt(15) + PriorityEncoder(Vec.tabulate(32-14-1) { i => !io.validEntries(i+14+1) })
    }
    is (UInt(15)) {
      io.pos(3) := UInt(16) + PriorityEncoder(Vec.tabulate(32-15-1) { i => !io.validEntries(i+15+1) })
    }
    is (UInt(16)) {
      io.pos(3) := UInt(17) + PriorityEncoder(Vec.tabulate(32-16-1) { i => !io.validEntries(i+16+1) })
    }
    is (UInt(17)) {
      io.pos(3) := UInt(18) + PriorityEncoder(Vec.tabulate(32-17-1) { i => !io.validEntries(i+17+1) })
    }
    is (UInt(18)) {
      io.pos(3) := UInt(19) + PriorityEncoder(Vec.tabulate(32-18-1) { i => !io.validEntries(i+18+1) })
    }
    is (UInt(19)) {
      io.pos(3) := UInt(20) + PriorityEncoder(Vec.tabulate(32-19-1) { i => !io.validEntries(i+19+1) })
    }
    is (UInt(20)) {
      io.pos(3) := UInt(21) + PriorityEncoder(Vec.tabulate(32-20-1) { i => !io.validEntries(i+20+1) })
    }
    is (UInt(21)) {
      io.pos(3) := UInt(22) + PriorityEncoder(Vec.tabulate(32-21-1) { i => !io.validEntries(i+21+1) })
    }
    is (UInt(22)) {
      io.pos(3) := UInt(23) + PriorityEncoder(Vec.tabulate(32-22-1) { i => !io.validEntries(i+22+1) })
    }
    is (UInt(23)) {
      io.pos(3) := UInt(24) + PriorityEncoder(Vec.tabulate(32-23-1) { i => !io.validEntries(i+23+1) })
    }
    is (UInt(24)) {
      io.pos(3) := UInt(25) + PriorityEncoder(Vec.tabulate(32-24-1) { i => !io.validEntries(i+24+1) })
    }
    is (UInt(25)) {
      io.pos(3) := UInt(26) + PriorityEncoder(Vec.tabulate(32-25-1) { i => !io.validEntries(i+25+1) })
    }
    is (UInt(26)) {
      io.pos(3) := UInt(27) + PriorityEncoder(Vec.tabulate(32-26-1) { i => !io.validEntries(i+26+1) })
    }
    is (UInt(27)) {
      io.pos(3) := UInt(28) + PriorityEncoder(Vec.tabulate(32-27-1) { i => !io.validEntries(i+27+1) })
    }
    is (UInt(28)) {
      io.pos(3) := UInt(29) + PriorityEncoder(Vec.tabulate(32-28-1) { i => !io.validEntries(i+28+1) })
    }
    is (UInt(29)) {
      io.pos(3) := UInt(30) + PriorityEncoder(Vec.tabulate(32-29-1) { i => !io.validEntries(i+29+1) })
    }
    is (UInt(30)) {
      io.pos(3) := UInt(31) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !io.validEntries(i+30+1) })
    }
    is (UInt(31)) {
      io.pos(3) := UInt(0) + PriorityEncoder(Vec.tabulate(32-30-1) { i => !io.validEntries(i+30+1) })
    }
    is (UInt(0)) {
      io.pos(3) := UInt(1) + PriorityEncoder(Vec.tabulate(32-0-1) { i => !io.validEntries(i+0+1) })
    }
  }
}

class AddrQueueAllocTests(c: AddrQueueAlloc) extends Tester(c) {
  println("TODO")
}

class AddrQueueAllocGenerator extends TestGenerator {
  def genMod(): Module = Module(new AddrQueueAlloc())
  def genTest[T <: Module](c: T): Tester[T] =
    (new AddrQueueAllocTests(c.asInstanceOf[AddrQueueAlloc])).asInstanceOf[Tester[T]]
}
