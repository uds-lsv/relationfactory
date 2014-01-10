#!/bin/sh
#<candidates.pb> <candidates>

COMP=$TAC_ROOT/components/pipeline/
JAVA_HOME=$TAC_ROOT/lib/java/jdk1.6.0_18/

$JAVA_HOME/bin/java -cp $COMP/dist/components.jar:$COMP/lib/* \
run.ProtoToCandidates $1 $2
