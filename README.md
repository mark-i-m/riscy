Project Riscy [![Build Status](https://travis-ci.com/mark-i-m/rocketchip.svg?token=z7ydGWURgxF6b4fcVtfN&branch=master)](https://travis-ci.com/mark-i-m/rocketchip)
=============

- Karan Bavishi
- Mark Mansi
- Suhas Pai
- Preyas Shah

The repo for our [CS
752](http://pages.cs.wisc.edu/~karu/courses/cs752/fall2016/wiki/index.php?n=Main.HomePage)
project! :smiley:

We are building a superscalar OOO RISCV processor optimized for high
performance rather than area or energy. In fact, while we tried to keep the
core synthesizeable, we are not really sure if it is due to the lack of
hardware structures like CAMs and register files in Chisel.

We made a number of hugely simplifying assumptions in order to finish on time:
- No virtual memory.
- No interrupts/traps/etc, though we do maintain mispeculation support, so
  adding these is no big deal.
- No memory-mapped I/O (simplifies LSQ).
- Only implementing the [RISCV RV64I
  instructions](https://riscv.org/specifications/), excluding memory fences.

Specs:
- 4-wide pipeline
- Out of Order execute, in-order commit
- Fetch
    - Blocking I$
    - TODO: size? associativity?
    - TODO: branch predictor?
- Renaming
    - ROB based
    - 64 entry ROB
- Issue/FUs
    - 4-wide issue
    - 4 issue queues of 16 entries, each feeding a single ALU
    - Load balancing arbiter selects which issue queue to insert an instruction into
- Load/Store Queue
    - 32 entry LSQ
    - Non-speculative memory disambiguation
    - Loads can issue out of order between stores
    - Stores only happen when they commit
    - TODO: D$
- Writeback
    - Processor supports back-to-back execution of dependent instructions
    - Writeback structure (a.k.a ROB WB or more affectionately, `FooPP`) is
      designed to avoid the massive tangle of wires created by broadcast-based
      writeback among 4 ALUs and a LSQ.
- Commit
    - 4-wide commit

A diagram of the whole pipeline can be found in the `doc` directory.

Our implementation was originally going to be based off of the rocket-chip
implementation from [UC Berkeley](https://github.com/ucb-bar/rocket-chip). We
ended up not doing this because the rocket core is not commented at all -> very
hard to understand.

Our implementation is entirely contained in the `riscy` subdirectory of this
repository, and the rocket core implementation was kept mostly for reference.

To build:
```
$ cd riscy
$ make test
```

Note that by "build", we mean generate, compile, and run C++ from the Chisel.
This C++ includes an extensive module-level test suite. You will need about 4GB
to compile (thanks to g++).

To run top-level tests (running some simple programs through the processor end-to-end):
```
$ cd riscy
$ make <bench>.hex.bench
```

where `<bench>` is the name of the benchmark in the `bench` directory.
Unfortunately, because of the lack of OS support by our processor, we cannot
run real RISCV programs, even though we implement enough of the ISA to actually
run most programs.
