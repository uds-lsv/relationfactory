#!/bin/bash
response=$1
key=$2
optargs="${@:3}"


java -cp $TAC_ROOT/evaluation/bin/ SFScore $response $key $optargs \
| grep -P '\tRecall:|\tPrecision:|\tF1:'

