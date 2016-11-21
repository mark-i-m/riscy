package riscy

import Chisel._

class SaturatingCounter(max: Int) {
  private val counter = new MultiCounter(max)

  val value = counter.value

  def up() = when(counter.value < UInt(max)) {
    counter.inc(1)
  }

  def down() = when(counter.value > UInt(0)) {
    counter.dec(1)
  }
}
