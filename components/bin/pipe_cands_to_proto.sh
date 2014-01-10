#!/bin/sh
# pipe_cands_to_proto < /dev/stdin > /dev/stdout
# /dev/stdin:  <candidates> 
# /dev/stdout: <candidates.pb>

COMP=$TAC_ROOT/components/pipeline/
JAVA_HOME=$TAC_ROOT/lib/java/jdk1.6.0_18/

$JAVA_HOME/bin/java -cp $COMP/dist/components.jar:$COMP/lib/* \
run.CandidatesToProto /dev/stdin /home/beroth/libs/opennlp/models/en-pos-maxent.bin /dev/stdout
