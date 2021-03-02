SBT_OPTS ?= -ea -Xmx1536M -Xms512M
SBT := $(shell which sbt 2>/dev/null > /dev/null && echo sbt || { if [ ! -f .sbt-launch.jar ]; then curl -o .sbt-launch.jar http://static.matthewfl.com/downloads/sbt-launch.jar; fi;   echo 'java ${SBT_OPTS} -jar .sbt-launch.jar' ; })

SOURCES=$(wildecard src/**/*.scala) $(wildecard src/**/*.java)

.PHONY: all clean

all: .compile_tracker

clean:
	rm -f .compile_tracker
	$(SBT) clean


.compile_tracker: $(SOURCES)
	$(SBT) compile
	touch .compile_tracker
