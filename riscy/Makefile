MODULES=HelloModule Fetch Decode Alloc ROB RegFile OpDecode ICache

TOP = ..
SBT ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -jar $(TOP)/sbt-launch.jar

TESTS=$(patsubst %, %.test, $(MODULES))

$(TESTS) : %.test :
	$(SBT) "run Test riscy.$*Generator --backend --genHarness --compile --test --vcd"

clean: 
	rm -f *.h *.cpp *.o
