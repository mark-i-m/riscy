# List of MODULES for unit tests
# Front end
MODULES = Fetch ICache DecodeSingle OpDecode Alloc ROB 
# Backend
MODULES += IssueQueue IqArbiter LSQ ALU RobWriteback Execute Stall
# Util
MODULES += HelloModule MultiCounter CAM RegFile Memory DCache AddrQueueAlloc 
MODULES += NBDCache 
###############################################################################
# Some configurations
TOP = ..
SBT ?= java -Xmx8G -Xss8M -XX:MaxPermSize=256M -jar $(TOP)/sbt-launch.jar

###############################################################################
# Rules for running benchmarks and unit tests

SCALA_FILES=$(wildcard src/main/scala/riscy/*.scala)
TESTS=$(patsubst %, %.test, $(MODULES))
BENCHES=$(patsubst bench/%, %.bench, $(wildcard bench/*.hex))
CHECKS=$(patsubst bench/%, %.check, $(wildcard bench/*.hex))

TMPPASSFILE := $(shell mktemp)
TMPFAILFILE := $(shell mktemp)

# Target to run all tests
test: $(TESTS)

bench: $(BENCHES)

check: $(CHECKS) print_pass_fail

# Run unit tests with `make <Module name>.test`
$(TESTS) : %.test :
	$(SBT) "run riscy.$*Generator"

Issue.test:
	$(SBT) "run riscy.IssueGenerator"

TopLevel.test:
	$(SBT) "run riscy.TopLevelGenerator"

# Run benchmarks with `make <Bench name>.bench`
$(BENCHES) : %.bench : ./riscy_emu
	@echo "\033[93mTEST $*\033[0m"
	@./riscy_emu bench/$* $*.log $*.vcd

# Run %.bench and compare the output with bench/%.ok
$(CHECKS) : %.check : %.bench
	@./check_output.py $*.log bench/$*.ok && \
		(echo "$*" >> $(TMPPASSFILE)) || (echo "$*" >> $(TMPFAILFILE))

# Build the emulator
emulator.h Riscy.h Riscy.cpp: $(SCALA_FILES)
	$(SBT) run

riscy_emu: riscy_emu.cc emulator.h Riscy.h Riscy.cpp
	g++ -ggdb -O0 -std=c++11 -o riscy_emu $^
	date

print_pass_fail:
	@echo "\033[93m================================================================================"
	@echo "                                    SUMMARY"
	@echo "================================================================================\033[0m\033[92m"
	@echo "`cat $(TMPPASSFILE) | wc -l` TESTS PASSED"
	@cat $(TMPPASSFILE) | sed 's/^/  /'
	@echo "\033[0m\033[91m"
	@echo "`cat $(TMPFAILFILE) | wc -l` TESTS FAILED"
	@cat $(TMPFAILFILE) | sed 's/^/  /'
	@echo "\033[0m"
	@if [ -s $(TMPFAILFILE) ] ; then echo '\033[93mSome tests failed :(\033[0m' ; else \
		echo '\033[92m\033[1mALL TESTS PASSED \(^o^)/\033[0m' ; fi

# Clean up!
clean: 
	rm -f *.h *.cpp *.o *.vcd *.log riscy_emu
	find -type f -executable ! -name check_output.py -exec rm -f {} \;
