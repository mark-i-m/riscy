
TOP = ../../../
SBT ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -jar $(TOP)/sbt-launch.jar

test :
	# Assume that for all modules X the Tester class is called TestX
	$(SBT) "run Test ${MODULE} ${TEST_CLASS} --test"
