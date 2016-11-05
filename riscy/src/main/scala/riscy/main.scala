package riscy

import Chisel._

object RiscyMain extends App {
  if (args.length > 0) {
    // To run unit tests, pass in the name of the module to test
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test", "--vcd")
    val generatorName = args(0)
    val gen = Class.forName(generatorName)
      .newInstance
      .asInstanceOf[TestGenerator]

    chiselMainTest(margs, gen.genMod) (gen.genTest) 
  } else {
    // Otherwise, just produce C++ for the whole chip
    chiselMain(Array("--backend", "c", "--vcd"), () => Module(new Riscy()))
  }
}
