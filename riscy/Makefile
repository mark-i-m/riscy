# List of MODULES for unit tests
# Front end
MODULES = Fetch ICache DecodeSingle OpDecode Alloc ROB 
# Backend
MODULES += IssueQueue IqArbiter LSQ Issue ALU RobWriteback Execute Stall
# Util
MODULES += HelloModule MultiCounter CAM RegFile Memory DCache AddrQueueAlloc

###############################################################################
# Some configurations
TOP = ..
SBT ?= java -Xmx4G -Xss8M -XX:MaxPermSize=256M -jar $(TOP)/sbt-launch.jar

###############################################################################
# Rules for running benchmarks and unit tests

SCALA_FILES=$(wildcard src/main/scala/riscy/*.scala)
TESTS=$(patsubst %, %.test, $(MODULES))
BENCHES=$(patsubst bench/%, %.bench, $(wildcard bench/*.hex))
CHECKS=$(patsubst bench/%, %.check, $(wildcard bench/*.hex))

# Target to run all tests
test: $(TESTS)

bench: $(BENCHES)

check: $(CHECKS)

# Run unit tests with `make <Module name>.test`
$(TESTS) : %.test :
	$(SBT) "run riscy.$*Generator"

TopLevel.test:
	$(SBT) "run riscy.TopLevelGenerator"

# Run benchmarks with `make <Bench name>.bench`
$(BENCHES) : %.bench : ./riscy_emu
	./riscy_emu bench/$* $*.log $*.vcd

# Run %.bench and compare the output with bench/%.ok
$(CHECKS) : %.check : %.bench
	./check_output.py $*.log bench/$*.ok

# Build the emulator
emulator.h Riscy.h Riscy.cpp: $(SCALA_FILES)
	$(SBT) run

riscy_emu: riscy_emu.cc emulator.h Riscy.h Riscy.cpp
	g++ -ggdb -O0 -std=c++11 -o riscy_emu $^

# Clean up!
clean: 
	rm -f *.h *.cpp *.o *.vcd *.log riscy_emu
	find -type f -executable ! -name check_output.py -exec rm -f {} \;
