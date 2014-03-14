#!/bin/sh
LOG4J=file://$TAC_ROOT/config/log4j.config

COMP=$TAC_ROOT/components/pipeline/
#JAVA_HOME=$TAC_ROOT/lib/java/jdk1.6.0_18/
java -Dfile.encoding=UTF8 -Dlog4j.configuration=$LOG4J -cp $COMP/dist/components.jar:$COMP/lib/* -Xmx32g $@
