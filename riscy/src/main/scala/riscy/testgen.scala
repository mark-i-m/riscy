package riscy

import Chisel._

trait TestGenerator {
  def genMod(): Module
  def genTest[T <: Module](c: T): Tester[T]
}
