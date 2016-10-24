package riscy

import Chisel._

object IcParams {
  val nSets = 64
  val nWays = 2
  val addr_width = 32
  val insts_per_cache_line = 8
  val cache_line_width = addr_width * insts_per_cache_line

  val tag_bits = 21
  val index_bits = 6
  val offset_bits = 5
}

class DummyMem extends Bundle {
  val datablock = UInt(OUTPUT, 32*8)
  val ready = Bool(OUTPUT)
  ready := Bool(true)
  datablock := UInt("h7777777766666666555555554444444433333333222222221111111100000000")
}

class ICacheReq extends Bundle {
  // TODO kbavishi: Parametrize the width
  val valid = Bool(INPUT)
  val addr = UInt(INPUT, width = IcParams.addr_width)
}

// Emits out a cache line.
class ICacheResp extends Bundle {
  //val ready = Bool(INPUT)
  //val valid = Bool(OUTPUT)
  val datablock = Bits(width = IcParams.cache_line_width/2)
}

class ICache extends Module {
  val io = new Bundle {
    val req = new ICacheReq
    val resp = Decoupled(new ICacheResp)
    val mem = new DummyMem
  }
  require(isPow2(IcParams.nSets) && isPow2(IcParams.nWays))

  val s_ready :: s_request :: s_refill_wait :: s_refill_done :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  // Used by frontend to stall the cache output
  val stall = !io.resp.ready
  // Used by cache to determine if it is ready for access
  val rdy = Wire(Bool())

  val s1_valid = Reg(init=Bool(false))
  val s1_vaddr = Reg(UInt(width = IcParams.addr_width))

  val tag_array = SeqMem(IcParams.nSets, Vec(IcParams.nWays, Bits(width = IcParams.tag_bits)))
  val data_array = SeqMem(IcParams.nSets, Vec(IcParams.nWays, Bits(width = IcParams.cache_line_width)))
  val vb_array = Reg(init=Bits(0, IcParams.nSets*IcParams.nWays))

  // SeqMem in Chisel returns after just one cycle. We want two cycles though
  val s1_tag_match = Wire(Vec(IcParams.nWays, Bool()))
  val s1_tag_hit = Wire(Vec(IcParams.nWays, Bool()))
  val s1_any_tag_hit = Wire(Bool())
  val s1_hit = Wire(Bool())
  val s1_miss = Wire(Bool())
  val s1_dout = Wire(Vec(IcParams.nWays, Bits(width = IcParams.cache_line_width)))

  // output signals - Two cycle latency
  val s2_valid = Reg(init=Bool(false))
  val s2_hit = Reg(init=Bool(false))
  val s2_tag_hit = Vec(IcParams.nWays, Reg(init=Bool(false)))
  val s2_dout = Vec(IcParams.nWays, Reg(UInt(width = IcParams.cache_line_width/2)))

  // Time starts now!
  s1_hit := s1_valid && s1_any_tag_hit
  s1_miss := s1_valid && !s1_any_tag_hit
  rdy := state === s_ready && !s1_miss

  io.resp.bits.datablock := Mux1H(s2_tag_hit, s2_dout)
  io.resp.valid := s2_hit

  val s0_valid = Mux(io.req.valid && rdy, io.req.valid, s1_valid)
  val s0_vaddr = Mux(io.req.valid && rdy, io.req.addr, s1_vaddr)
  val s0_idx = s0_vaddr(IcParams.offset_bits+IcParams.index_bits-1,
                        IcParams.offset_bits)

  val s1_tag = s1_vaddr(IcParams.addr_width-1,
                        IcParams.offset_bits+IcParams.index_bits)
  val s1_idx = s1_vaddr(IcParams.offset_bits+IcParams.index_bits-1,
                        IcParams.offset_bits)
  val s1_offset = s1_vaddr(IcParams.offset_bits-1, 0)

  // Check if we got a miss. If we did, we need to refill from memory
  val refill_addr = Reg(UInt(width = IcParams.addr_width))
  when (s1_miss && state === s_ready) {
    refill_addr := s1_vaddr
  }

  val refill_tag = refill_addr(IcParams.addr_width-1,
                               IcParams.offset_bits+IcParams.index_bits)
  val refill_idx = refill_addr(IcParams.offset_bits+IcParams.index_bits-1,
                               IcParams.offset_bits)
  val repl_way = LFSR16(s1_miss)(log2Up(IcParams.nWays)-1,0)
  when (state === s_refill_wait) {
    // TODO kbavishi: Need a smarter way to update the correct way
    tag_array.write(refill_idx, Vec.fill(IcParams.nWays)(refill_tag))
    data_array.write(refill_idx, Vec.fill(IcParams.nWays)(io.mem.datablock))
    vb_array := vb_array.bitSet(Cat(Bits(0), refill_idx), Bool(true))
  }

  val refill_in_progress = state === s_refill_wait || state === s_refill_done

  // Perform tag array match
  val tag_rdata = tag_array.read(s0_idx, s0_valid && !refill_in_progress)
  for (i <- 0 until IcParams.nWays) {
    val s1_tag_vb = vb_array(Cat(UInt(i), s1_idx)).toBool
    val tag_out = tag_rdata(i)
    s1_tag_match(i) := tag_out(IcParams.tag_bits-1,0) === s1_tag
    s1_tag_hit(i) := s1_tag_vb && s1_tag_match(i) && !refill_in_progress
  }
  s1_any_tag_hit := s1_tag_hit.reduceLeft(_||_)

  // Perform data array match
  s1_dout := data_array.read(s0_idx, s0_valid && !refill_in_progress)

  // Perform updates useful for next cycle
  // Don't forward s1_valid to s2_valid if cache is not access-ready
  //s2_valid := s1_valid && rdy
  s2_valid := s1_valid
  s2_hit := s1_hit
  s2_tag_hit := s1_tag_hit
  // Need to rotate data out here
  for (i <- 0 until IcParams.nWays) {
    switch (s1_offset) {
      is (Bits(0)) {
        s2_dout(i) := s1_dout(i)(127, 0)
      }
      is (Bits(4)) {
        s2_dout(i) := s1_dout(i)(159, 32)
      }
      is (Bits(8)) {
        s2_dout(i) := s1_dout(i)(191, 64)
      }
      is (Bits(12)) {
        s2_dout(i) := s1_dout(i)(223, 96)
      }
      is (Bits(16)) {
        s2_dout(i) := s1_dout(i)(255, 128)
      }
      is (Bits(20)) {
        s2_dout(i) := Cat(Bits(0), s1_dout(i)(255, 160))
      }
      is (Bits(24)) {
        s2_dout(i) := Cat(Bits(0), s1_dout(i)(255, 192))
      }
      is (Bits(28)) {
        s2_dout(i) := Cat(Bits(0), s1_dout(i)(255, 224))
      }
    }
  }

  s1_valid := s0_valid
  s1_vaddr := s0_vaddr

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s1_miss) { state := s_refill_wait }
    }
    is (s_request) {
    }
    is (s_refill_wait) {
      state := s_refill_done
    }
    is (s_refill_done) {
      state := s_ready
    }
  }
}

class ICacheTests(c: ICache) extends Tester(c) { 
  // Assume that the next stage (fetch) is always ready to receive
  poke(c.io.resp.ready, true)
  // Assume for now that request is always valid
  poke(c.io.req.valid, true)

  // Simple sanity testcases. Start requested PC from address zero. Keep
  // incrementing PC by 4. Verify that the ICache is pipelined ie. we get a
  // cache hit every cycle
  //
  // The first requested PC will see a total latency of 5 cycles: 2 normal
  // cycles for determining hit/miss, 2 cycles for refilling the cache by
  // DummyMem and 1 more cycle for accessing cache after refill. 
  //
  // Verify that from cycle #5 onwards, we get a hit every cycle
  poke(c.io.req.addr, 0)
  step(1)
  expect(c.io.resp.valid, false)

  step(1)
  expect(c.io.resp.valid, false)

  step(1)
  expect(c.io.resp.valid, false)
  // Before cycle #4 begins, put in a new request
  poke(c.io.req.addr, 4)

  step(1)
  expect(c.io.resp.valid, false)
  poke(c.io.req.addr, 8)

  for (i <- 3 until 8) {
    step(1)
    expect(c.io.resp.valid, true)
    peek(c.io.resp.bits.datablock)
    // Send new request
    poke(c.io.req.addr, i*4)
  }
  step(1)
  expect(c.io.resp.valid, true)
  peek(c.io.resp.bits.datablock)
  step(1)
  expect(c.io.resp.valid, true)
  peek(c.io.resp.bits.datablock)


  /*
   * Testcases to be added:
   * 1. Increment PC by 4W. Verify that we refill every other instruction and
   *    get a cache hit for the subsequent instruction
   * 2. Verify that cache is 2-way set associative somehow. Verify the tag
   *    overwriting maybe by requesting 3 instructions with the same index?
   * 
   * Small unit tests:
   * 1. Verify that the correct datablock is being fetched. Need to hack
   *    DummyMem code for this.
   * 2. Verify that the vb_array is being updated correctly.
   * 
   * Code to be added:
   * 1. Add random tag block replacement logic
   * 2. Add stall, s1_kill, s2_kill logic and testcases
   * 3. Add request invalid testcases.
   * 4. Add rotator logic. Add testcases for cache line boundary crossing
   *    requests
   *
   */
} 

class ICacheGenerator extends TestGenerator {
  def genMod(): Module = Module(new ICache())
  def genTest[T <: Module](c: T): Tester[T] = 
    (new ICacheTests(c.asInstanceOf[ICache])).asInstanceOf[Tester[T]]
}
