package riscy

import Chisel._

object IcParams {
  val nSets = 64
  val nWays = 2
  val addr_width = 64
  val inst_width = 32
  val fetch_width = 4
  val insts_per_cache_line = fetch_width * 2
  val cache_line_width = inst_width * insts_per_cache_line

  val prefetch_buffer_len = 4
  val prefetch_offset_bits = 5
  val prefetch_tag_bits = addr_width - prefetch_offset_bits

  val index_bits = 6
  val offset_bits = 5
  val tag_bits = addr_width - index_bits - offset_bits
}

class ICacheReq extends Bundle {
  // TODO kbavishi: Parametrize the width
  val valid = Bool(INPUT)
  val addr = UInt(INPUT, width = IcParams.addr_width)
}

// Emits out a cache line.
class ICacheResp extends Bundle {
  // This signal can be used to stall Icache
  val stall = Bool(INPUT)
  // Indicates whether Icache encountered a hit.
  val valid = Bool(OUTPUT)
  // Indicates whether the Icache is available for accepting requests
  val idle = Bool(OUTPUT)
  // An array of four 32b instructions emitted out to the Decode stage
  val inst = Vec(IcParams.fetch_width, Bits(OUTPUT, width = IcParams.inst_width))

  // Debug signals
  // The original request addr for which the above 4 instructions were
  // generated. This is only needed for unit testing
  val addr = Bits(OUTPUT, width = IcParams.addr_width)
}

class ICache extends Module {
  val io = new Bundle {
    // Address to be requested
    val req = new ICacheReq
    // Use this to invalidate any requests previously issued
    val kill = Bool(INPUT)

    // Gives four valid instructions if possible. Valid bits are unset if four
    // valid instructions could not be found due to boundary issues.
    val resp = new ICacheResp

    // Interface with memory
    val memReadPort = Valid(UInt(OUTPUT, IcParams.addr_width)).asOutput
    val memReadData = Valid(UInt(INPUT, IcParams.cache_line_width *
                                        (IcParams.prefetch_buffer_len + 1))).asInput
    val memCancelPort = Valid(UInt(OUTPUT, IcParams.addr_width)).asOutput
  }
  require(isPow2(IcParams.nSets) && isPow2(IcParams.nWays))

  val s_ready :: s_request :: s_refill_init :: s_refill_wait :: s_refill_done :: Nil = Enum(UInt(), 5)
  val state = Reg(init=s_ready)
  // Used by frontend to stall the cache output
  val stall = io.resp.stall
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
  val s2_hit = Reg(init=Bool(false))
  val s2_prefetch_hit = Reg(init=Bool(false))
  val s2_miss = Reg(init=Bool(false))
  val s2_tag_hit = Vec(IcParams.nWays, Reg(init=Bool(false)))
  val s2_prefetch_tag_hit = Vec(IcParams.prefetch_buffer_len, Reg(init=Bool(false)))
  val s2_dout = Vec(IcParams.nWays, Reg(UInt(width = IcParams.cache_line_width/2)))
  val s2_prefetch_out = Reg(UInt(width = IcParams.cache_line_width/2))
  val s2_vaddr = Reg(UInt(0, width = IcParams.addr_width))

  // Prefetch related wires
  val s1_prefetch_tag_match = Vec(IcParams.prefetch_buffer_len, Bool())
  val s1_prefetch_tag_hit = Vec(IcParams.prefetch_buffer_len, Bool())
  val s1_any_prefetch_tag_hit = Wire(Bool())
  val s1_prefetch_hit = Wire(Bool())
  val s1_prefetch_out = Bits(width = IcParams.cache_line_width)

  // Circular buffer of 4 entries to store extra cache lines received from
  // memory. This is to basically simulate prefetching
  val prefetch_tag = Vec(IcParams.prefetch_buffer_len,
                         Reg(UInt(width = IcParams.prefetch_tag_bits)))
  val prefetch_line = Vec(IcParams.prefetch_buffer_len,
                          Reg(UInt(width = IcParams.cache_line_width)))
  val prefetch_valid = Vec(IcParams.prefetch_buffer_len, Reg(init=Bool(false)))

  // Time starts now!
  // We can say we got a "hit" if any of the tags matched in either the cache
  // tag array or the prefetch buffer.
  s1_hit := s1_valid && (s1_any_tag_hit || s1_any_prefetch_tag_hit) && !io.kill
  s1_miss := s1_valid && (!s1_any_tag_hit && !s1_any_prefetch_tag_hit) && !io.kill
  rdy := state === s_ready && !s1_miss

  // The final output will be either from the data array or the prefetch buffer
  val dout = Mux(s2_prefetch_hit, s2_prefetch_out, Mux1H(s2_tag_hit, s2_dout))
  for (i <- 0 until IcParams.fetch_width) {
    io.resp.inst(i) := dout(32*i+31, 32*i)
  }
  io.resp.valid := s2_hit
  // The Icache is deemed idle if it saw a hit or didn't see a miss.
  // This is basically useful for the first few cycles where we will have
  // neither a hit nor a miss.
  io.resp.idle := (s2_hit) || (!s2_miss && state === s_ready)
  io.resp.addr := s2_vaddr

  val s0_valid = Mux(rdy && !stall, io.req.valid, s1_valid && !io.kill)
  // These s0_* values represent the addresses to be checked in this cycle
  val s0_vaddr = Mux(rdy && !stall, io.req.addr, s1_vaddr)
  val s0_idx = s0_vaddr(IcParams.offset_bits+IcParams.index_bits-1,
                        IcParams.offset_bits)

  // These s1_* values represent the addresses that were checked 1 cycle ago
  val s1_tag = s1_vaddr(IcParams.addr_width-1,
                        IcParams.offset_bits+IcParams.index_bits)
  val s1_idx = s1_vaddr(IcParams.offset_bits+IcParams.index_bits-1,
                        IcParams.offset_bits)
  val s1_offset = s1_vaddr(IcParams.offset_bits-1, 0)
  // This wire will be needed for checking the prefetch buffer.
  val s1_pf_tag = s1_vaddr(IcParams.addr_width-1,
                           IcParams.prefetch_offset_bits)

  // Check if we got a miss. If we did, we need to refill from memory
  val refill_addr = Reg(UInt(width = IcParams.addr_width))
  when (s1_miss && state === s_ready) {
    // s1_vaddr contains the address for which the data and tag arrays were
    // consulted 1 cycle ago and it generated a miss.
    refill_addr := s1_vaddr
  }

  // Refill logic
  val refill_tag = refill_addr(IcParams.addr_width-1,
                               IcParams.offset_bits+IcParams.index_bits)
  val refill_idx = refill_addr(IcParams.offset_bits+IcParams.index_bits-1,
                               IcParams.offset_bits)
  val replace_way = LFSR16(s1_miss)(0)

  // Seems like we are waiting for refill and the memory has responded. Update
  // the tag and data arrays.
  when (state === s_refill_wait && io.memReadData.valid && !io.kill) {
    // Our cache line replacement policy is random replacement
    tag_array.write(refill_idx, Vec.fill(IcParams.nWays)(refill_tag),
                    Vec.tabulate(IcParams.nWays)(replace_way === Bits(_)))
    data_array.write(refill_idx,
      Vec.fill(IcParams.nWays)(io.memReadData.bits(IcParams.cache_line_width-1,0)))
    vb_array := vb_array.bitSet(Cat(replace_way, refill_idx), Bool(true))

    // Also fill the prefetch buffer with 4 additional lines
    for (i <- 0 until IcParams.prefetch_buffer_len) {
      prefetch_line(i) := io.memReadData.bits(IcParams.cache_line_width * (i+2) - 1,
                                              IcParams.cache_line_width * (i+1))
      prefetch_valid(i) := Bool(true)
      prefetch_tag(i)   := refill_addr(IcParams.addr_width-1,
                                       IcParams.prefetch_offset_bits) + UInt(i+1)
    }
  }

  val refill_in_progress = state === s_refill_init || 
                           state === s_refill_wait || 
                           state === s_refill_done

  // Initiate a memory request if we encountered a miss
  when(state === s_refill_init && !io.kill) {
    io.memReadPort.valid := Bool(true)
    io.memReadPort.bits  := refill_addr
  } .otherwise {
    io.memReadPort.valid := Bool(false)
    io.memReadPort.bits  := UInt(0)
  }

  // Cancel a memory request if kill was requested
  when(state === s_refill_wait && io.kill) {
    io.memCancelPort.valid := Bool(true)
    io.memCancelPort.bits  := refill_addr
  } .otherwise {
    io.memCancelPort.valid := Bool(false)
    io.memCancelPort.bits  := UInt(0)
  }

  // Perform tag array match
  val tag_rdata = tag_array.read(s0_idx, s0_valid && !refill_in_progress)
  for (i <- 0 until IcParams.nWays) {
    val s1_tag_vb = vb_array(Cat(UInt(i), s1_idx)).toBool
    val tag_out = tag_rdata(i)
    s1_tag_match(i) := tag_out(IcParams.tag_bits-1,0) === s1_tag
    s1_tag_hit(i) := s1_tag_vb && s1_tag_match(i) && !refill_in_progress
  }
  s1_any_tag_hit := s1_tag_hit.reduceLeft(_||_)

  // Perform data array match in parallel
  s1_dout := data_array.read(s0_idx, s0_valid && !refill_in_progress)

  // Also perform tag match against prefetch buffer tags in parallel
  for (i <- 0 until IcParams.prefetch_buffer_len) {
    s1_prefetch_tag_match(i) := prefetch_tag(i) === s1_pf_tag
    s1_prefetch_tag_hit(i)   := (prefetch_valid(i) && s1_prefetch_tag_match(i) &&
                                 !refill_in_progress)
  }
  s1_any_prefetch_tag_hit := s1_prefetch_tag_hit.reduceLeft(_||_)
  s1_prefetch_hit  := s1_valid && s1_any_prefetch_tag_hit && !io.kill

  // Perform prefetch data match in parallel
  s1_prefetch_out  := Mux1H(s1_prefetch_tag_hit, prefetch_line)

  // Perform updates useful for next cycle
  when(!stall) {
    s2_hit              := s1_hit && !io.kill
    s2_prefetch_hit     := s1_prefetch_hit && !io.kill
    s2_miss             := s1_miss && !io.kill
    s2_tag_hit          := s1_tag_hit
    s2_prefetch_tag_hit := s1_prefetch_tag_hit
    s2_vaddr            := s1_vaddr
  }

  // Need to rotate data array output here
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
  // Rotate prefetch buffer output
  switch (s1_offset) {
    is (Bits(0)) {
      s2_prefetch_out := s1_prefetch_out(127, 0)
    }
    is (Bits(4)) {
      s2_prefetch_out := s1_prefetch_out(159, 32)
    }
    is (Bits(8)) {
      s2_prefetch_out := s1_prefetch_out(191, 64)
    }
    is (Bits(12)) {
      s2_prefetch_out := s1_prefetch_out(223, 96)
    }
    is (Bits(16)) {
      s2_prefetch_out := s1_prefetch_out(255, 128)
    }
    is (Bits(20)) {
      s2_prefetch_out := Cat(Bits(0), s1_prefetch_out(255, 160))
    }
    is (Bits(24)) {
      s2_prefetch_out := Cat(Bits(0), s1_prefetch_out(255, 192))
    }
    is (Bits(28)) {
      s2_prefetch_out := Cat(Bits(0), s1_prefetch_out(255, 224))
    }
  }

  when (!stall) {
    s1_valid := s0_valid
    s1_vaddr := s0_vaddr
  } .otherwise {
    s1_valid := s1_valid
    s1_vaddr := s1_vaddr
  }

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s1_miss) { state := s_refill_init }
    }
    is (s_request) {
    }
    is (s_refill_init) {
      when (io.kill) { 
        state := s_ready
      } .otherwise {
        state := s_refill_wait
      }
    }
    is (s_refill_wait) {
      when (io.memReadData.valid && !io.kill) { state := s_refill_done }
      when (io.kill) { state := s_ready }
    }
    is (s_refill_done) {
      state := s_ready
    }
  }
}

class ICacheTests(c: ICache) extends Tester(c) { 
  // Assume that the next stage (fetch) is always ready to receive
  poke(c.io.resp.stall, false)
  // Assume for now that request is always valid
  poke(c.io.req.valid, true)

  // Simple sanity testcases. Start requested PC from address zero. Keep
  // incrementing PC by 4. Verify that the ICache is pipelined ie. we get a
  // cache hit every cycle
  //
  // The first requested PC will see a total latency of 5 cycles: 2 normal
  // cycles for determining hit/miss, 3 cycles for refilling the cache
  // and 1 more cycle for accessing cache after refill. 
  //
  // Verify that from cycle #5 onwards, we get a hit every cycle.
 
  // We expect the Icache to be ready to receive requests at this point
  expect(c.io.resp.idle, true)
  poke(c.io.req.addr, 0x0)
  expect(c.io.memReadPort.valid, false)
  step(1)

  // Cycle 1
  peek(c.s2_miss)
  peek(c.state)
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, true)
  expect(c.io.memReadPort.valid, false)
  step(1)

  // Cycle 2
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)

  // Memory request from I$
  expect(c.io.memReadPort.valid, true)
  expect(c.io.memReadPort.bits, 0x0)
  step(1)

  // Cycle 3
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)
  expect(c.io.memReadPort.valid, false)

  // Memory fullfills I$ request
  poke(c.io.memReadData.valid, true)
  poke(c.io.memReadData.bits, 0)
  step(2)

  // Cycle 5
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)
  expect(c.io.memReadPort.valid, false)
  // Before cycle #5 begins, put in a new request
  poke(c.io.req.addr, 0x4)
  step(1)

  for (i <- 2 until 40) {
    expect(c.io.resp.valid, true)
    expect(c.io.resp.idle, true)
    expect(c.io.resp.addr, (i-2)*4)
    expect(c.io.memReadPort.valid, false)
    peek(c.io.resp.inst)
    // Send new request
    poke(c.io.req.addr, i*4)
    step(1)
  }
  // Cycle 44
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 152)
  expect(c.io.memReadPort.valid, false)
  peek(c.io.resp.inst)
  poke(c.io.req.addr, 160)
  step(1)

  // Cycle 45
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 156)
  expect(c.io.memReadPort.valid, false)
  peek(c.io.resp.inst)
  poke(c.io.req.addr, 164)
  step(1)

  // Cycle 46 - We expect a miss because of the request for addr 160.
  // This will take 3 cycles to refill and 1 more cycle to get result
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)

  // Memory request from I$
  expect(c.io.memReadPort.valid, true)
  expect(c.io.memReadPort.bits, 160)
  peek(c.io.resp.inst)
  step(1)

  // Cycle 47 - Refill going on
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)

  // Memory fullfills I$ request
  poke(c.io.memReadData.valid, true)
  poke(c.io.memReadData.bits, 0)
  peek(c.io.resp.inst)
  step(2)

  // Cycle 49 - Refill finished. Should get hit in next cycle
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)
  peek(c.io.resp.inst)
  poke(c.io.req.addr, 164)
  step(1)

  // Cycle 50 - Should get hit.
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 160)
  peek(c.io.resp.inst)
  // Lets stall the Icache.
  poke(c.io.resp.stall, true)
  poke(c.io.req.addr, 168)
  step(1)

  // Cycle 51 - The response should be held since Icache is stalled
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 160)
  peek(c.io.resp.inst)
  // Unstall and verify that the previous response wasn't lost
  poke(c.io.resp.stall, false)
  step(1)

  // Cycle 52 - Should get hit for 164 since we are unstalled.
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 164)
  peek(c.io.resp.inst)
  // Kill requests issued before 
  poke(c.io.kill, true)
  poke(c.io.req.addr, 4)
  step(1)

  // Cycle 53 - Response for addr 168 should be invalidated because of our kill
  // request
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 168)
  peek(c.io.resp.inst)
  poke(c.io.kill, false)
  poke(c.io.req.addr, 8)
  step(1)

  // Cycle 54 - Response for addr 4 should be valid and a hit
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 4)
  peek(c.io.resp.inst)
  poke(c.io.req.addr, 12)
  step(1)

  // Cycle 55 - Verify that we receive a response for addr 8
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 8)
  peek(c.io.resp.inst)
  // Request address which will generate a refill request
  poke(c.io.req.addr, 1000)
  step(1)

  // Cycle 56 - Response for addr 12 should be received correctly
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 12)
  peek(c.io.resp.inst)
  // Issue request for an address that will be a hit
  poke(c.io.req.addr, 0)
  step(1)

  // Cycle 57 - No response should be available for addr 1000. Verify that
  // refill request is not made if kill is issued
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)
  // Should be in refill_init state
  expect(c.state, 2)
  // Issue kill request. 
  poke(c.io.kill, true)
  expect(c.io.memReadPort.valid, false)
  // Verify that we don't issue a cancel request either
  expect(c.io.memCancelPort.valid, false)
  step(1)

  // Cycle 58 - Should be ready to accept one request
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, true)
  expect(c.state, 0)
  poke(c.io.kill, false)
  // Pick address which will generate another refill request
  poke(c.io.req.addr, 2000)
  step(1)

  // Cycle 59 - Should be ready to accept one more request
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, true)
  poke(c.io.req.addr, 0)
  step(1)

  // Cycle 60 - Icache should generate a refill request
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)
  // Should be in refill_init state
  expect(c.state, 2)
  expect(c.io.memReadPort.valid, true)
  expect(c.io.memReadPort.bits, 2000)
  step(1)

  // Cycle 61 - Icache will be waiting for mem response. Verify that it
  // responds to kill requests even in this state and cancels any previously
  // issued refill requests.
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, false)
  // Should be in refill_wait state
  expect(c.state, 3)
  // Issue kill request. 
  poke(c.io.kill, true)
  expect(c.io.memCancelPort.valid, true)
  expect(c.io.memCancelPort.bits, 2000)
  step(1)

  // Cycle 62 - Icache should be able to take a request
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, true)
  poke(c.io.kill, false)
  poke(c.io.req.addr, 0)
  step(1)

  // Cycle 63 - Icache should be able to take another request
  expect(c.io.resp.valid, false)
  expect(c.io.resp.idle, true)
  poke(c.io.req.addr, 4)
  step(1)

  // Cycle 64 - Should be a hit
  expect(c.io.resp.valid, true)
  expect(c.io.resp.idle, true)
  expect(c.io.resp.addr, 0)
  step(1)
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
   * 2. Add request invalid testcases.
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
