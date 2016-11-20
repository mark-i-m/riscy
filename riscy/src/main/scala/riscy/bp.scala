package riscy

import Chisel._

// RAS copied from the Rocket Chip
class RAS(nras: Int) {
  private val count = Reg(UInt(width = log2Up(nras+1)))
  private val pos = Reg(UInt(width = log2Up(nras)))
  private val stack = Reg(Vec(nras, UInt()))

  def push(addr: UInt): Unit = {
    when (count < nras) { count := count + 1 }
    val nextPos = Mux(Bool(isPow2(nras)) || pos < nras-1, pos+1, UInt(0))
    stack(nextPos) := addr
    pos := nextPos
  }
  def peek: UInt = stack(pos)
  def pop(): Unit = when (!isEmpty) {
    count := count - 1
    pos := Mux(Bool(isPow2(nras)) || pos > 0, pos-1, UInt(nras-1))
  }
  def clear(): Unit = count := UInt(0)
  def isEmpty: Bool = count === UInt(0)
}

// A table of 2-bit counters and a global history register
// - nbht table entries
// - hbits of history
//
// You need to check the BTB to see if a PC corresponds to branch. The BHT
// will tell you whether it is predicted taken on not taken.
class BHT(nbht: Int) {
  private val numhist = log2Up(nbht)

  private val table = Array.fill(4) { new SaturatingCounter(3) }
  private val history = Reg(UInt(width = numhist))

  // Hash takes the low-order bits of the PC and XORs with history
  // TODO: perhaps we want to experiment with this?
  private def hash(pc: UInt): UInt = pc(numhist, 0) ^ history

  // Returns a prediction: taken (1) or not taken (0)
  // TODO: should we return the history too? How to do update?
  // TODO: should we keep some state about which branches were
  // predicted taken?
  def get(pc: UInt): Bool = table(hash(pc)).value >= 2

  // TODO: we should not use the current hash! we should use the
  // hash at the time the jump was issued...
  def update(pc: UInt, taken: Bool) = when(taken) {
    table(hash(pc)).up
  } .otherwise {
    table(hash(pc)).down
  }
}

// A BTB: keeps track of which PCs are predicted to be branches.
class BTB(nbtb: Int) {
  // TODO: how to implement this? Is this a huge CAM?
}
