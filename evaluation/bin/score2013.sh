#!/bin/bash
# provenance invariant scorer for 2013 data
response=$1
key=$2
#optargs="${@:3}"


java -cp $TAC_ROOT/evaluation/bin/ SFScore $response $key anydoc \
| grep -P '\tRecall:|\tPrecision:|\tF1:'

