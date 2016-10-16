
TOP = ..
SBT ?= java -Xmx2G -Xss8M -XX:MaxPermSize=256M -jar $(TOP)/sbt-launch.jar

# Set the MODULE_GEN environment variable to the name of the module generator class
test :
	$(SBT) "run Test ${MODULE_GEN} --backend --genHarness --compile --test"
