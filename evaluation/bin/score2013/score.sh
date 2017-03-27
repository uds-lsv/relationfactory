#!/bin/bash
# provenance invariant scorer for 2013 data
response=$1
key=$2
optargs="${@:3}"


java -cp $TAC_ROOT/evaluation/bin/score2013/ SFScore $response $key anydoc $optargs \
| grep -P '\tRecall:|\tPrecision:|\tF1:'

