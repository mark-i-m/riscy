package riscy

import Chisel._

class HelloModule extends Module { 
  val io = new Bundle { 
    val out = UInt(OUTPUT, 8) 
  } 
  io.out := UInt(42) 
  printf("Hello, World!\n")
} 

class HelloModuleTests(c: HelloModule) extends Tester(c) { 
  step(1) 
  expect(c.io.out, 42) 
} 

class HelloModuleGenerator extends TestGenerator {
  def genMod(): Module = Module(new HelloModule())
  def genTest[T <: Module](c: T): Tester[T] = 
    (new HelloModuleTests(c.asInstanceOf[HelloModule])).asInstanceOf[Tester[T]]
}
