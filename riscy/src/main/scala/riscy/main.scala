package riscy

import Chisel._

object RiscyMain extends App {
  // To run RISCY unit tests, pass "Test" as the first argument, followed
  // by the name of the module/tests generator for the module we want to test.
  if (args(0).equals("Test")) {
    val generatorName = args(1)

    val gen = Class.forName(generatorName)
      .newInstance
      .asInstanceOf[TestGenerator]

    chiselMainTest(args.drop(3), gen.genMod) (gen.genTest) 

  } else {
    // TODO: chiselMain with the whole processor
  }
}
