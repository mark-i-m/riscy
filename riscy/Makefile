# List of MODULES for unit tests
MODULES = Fetch ICache DecodeSingle OpDecode Alloc ROB IssueQueue IqArbiter LSQ RobWriteback Stall
MODULES += HelloModule MultiCounter CAM RegFile ALU Issue TopLevel

# List of BENCHES for integration testing
BENCHES=Hello.hex.bench

###############################################################################
# Some configurations
TOP = ..
SBT ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -jar $(TOP)/sbt-launch.jar

###############################################################################
# Rules for running benchmarks and unit tests

SCALA_FILES=$(wildcard src/main/scala/riscy/*.scala)
TESTS=$(patsubst %, %.test, $(MODULES))

# Target to run all tests
test: $(TESTS)

# Run unit tests with `make <Module name>.test`
$(TESTS) : %.test :
	$(SBT) "run riscy.$*Generator"

# Run benchmarks with `make <Bench name>.bench`
$(BENCHES) : %.bench : ./riscy_emu
	./riscy_emu bench/$* $*.log $*.vcd

# Build the emulator
emulator.h Riscy.h Riscy.cpp: $(SCALA_FILES)
	$(SBT) run

riscy_emu: riscy_emu.cc emulator.h Riscy.h Riscy.cpp
	g++ -o riscy_emu -ggdb -O0 $^

# Clean up!
clean: 
	rm -f *.h *.cpp *.o *.vcd *.log riscy_emu
	find -type f -executable -exec rm -f {} \;
