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

Better documentation can be found in `doc/doc.tex`. To render: `pdflatex
doc.tex` in `doc`.

A diagram of the whole pipeline can be found in the `doc` directory.

Our implementation is entirely contained in the `riscy` subdirectory of this
repository.

To build and run module-level tests:
```
$ cd riscy
$ make test
```

To build and run top-level tests/benchmarks with the whole pipeline (this takes
a huge amount of memory [more than ~6GB]):
```
$ cd riscy
$ make check
```

Note that by "build", we mean generate, compile, and run C++ from the Chisel.
