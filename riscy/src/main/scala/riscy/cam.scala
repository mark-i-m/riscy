package riscy

import Chisel._

// A generic CAM Module. Info about ctor args:
//
// @numEntries - Specifies the number of entries that will be matched in
// parallel by the CAM
//
// @entryWidth - The width of each entry that will be matched by the CAM
//
class CAM(numCompare : Int, numEntries : Int, entryWidth : Int) extends Module {
  val io = new Bundle {
    // The value with which all the input values will be compared
    val compare_bits = Vec(numCompare, Bits(INPUT, width=entryWidth))
    // Plug in multiple values which will be compared in parallel with one
    // particular value
    val input_bits = Vec(numEntries, Bits(INPUT, width=entryWidth))
    // The hit bit will be set for any entry i if it matches with the compared
    // value.
    val hit = Vec(numCompare, (Vec(numEntries, Bool(OUTPUT))))
  }

  // Seems too simplistic, doesn't it?
  for (j <- 0 until numCompare) {
	  for (i <- 0 until numEntries) {
    	io.hit(j)(i) := (io.compare_bits(j) === io.input_bits(i))
  	}
	}
}

class CAMTests(c: CAM) extends Tester(c) { 
  // Testcase 1: Verify that CAM reports exactly 1 hit when only one entry
  // matches.
  println("Testcase 1")
  val compare_entry = 1
  poke(c.io.compare_bits(0), compare_entry)

  // Set the input values to be matched from 0 to 3
  for (i <- 0 until 4) {
    poke(c.io.input_bits(i), i)
  }
  // The hit bit should only be set for one matched entry 
  for (i <- 0 until 4) {
    if (i == compare_entry) {
      expect(c.io.hit(0)(i), 1)
    } else {
      expect(c.io.hit(0)(i), 0)
    }
  }

  // Testcase 2: Verify that CAM reports all hits when all entries match
  // Set the input values to be matched from 0 to 3
  println("Testcase 2")
  for (i <- 0 until 4) {
    poke(c.io.input_bits(i), compare_entry)
  }
  // The hit bit should be set for all entries
  for (i <- 0 until 4) {
    expect(c.io.hit(0)(i), 1)
  }

  // Testcase 3: Verify that CAM reports zero hits when no entries match
  println("Testcase 3")
  val mismatch_entry = 2
  for (i <- 0 until 4) {
    poke(c.io.input_bits(i), mismatch_entry)
  }
  // The hit bits should not be set for any entry
  for (i <- 0 until 4) {
    expect(c.io.hit(0)(i), 0)
  }

  // Testcase 4: Multiple hits testcase. Verify that CAM works when some
  // entries match and some don't.
  println("Testcase 4")
  for (i <- 0 until 4) {
    if (i < 2) {
      poke(c.io.input_bits(i), compare_entry)
    } else {
      poke(c.io.input_bits(i), mismatch_entry)
    }
  }
  // The hit bits should be set only for the matching entries
  for (i <- 0 until 4) {
    if (i < 2) {
      expect(c.io.hit(0)(i), 1)
    } else {
      expect(c.io.hit(0)(i), 0)
    }
  }
  
}

class CAMGenerator extends TestGenerator {
  def genMod(): Module = Module(new CAM(1, 4, 2))
  def genTest[T <: Module](c: T): Tester[T] = 
    (new CAMTests(c.asInstanceOf[CAM])).asInstanceOf[Tester[T]]
}
