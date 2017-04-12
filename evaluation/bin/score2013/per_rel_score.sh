#!/bin/bash
# provenance invariant scorer for 2013 data
response=$1
key=$2
optargs="${@:3}"
rellist=$TAC_ROOT/config/rellist2013

while read rel; do

echo $rel

slotlist=`mktemp`

cut -f1,2 $response \
| tr '\t' ':' \
| sort -u \
| grep $rel \
> $slotlist

java -cp $TAC_ROOT/evaluation/bin/score2013/ SFScore $response $key anydoc slots=$slotlist $optargs \
| grep -P '\tRecall:|\tPrecision:|\tF1:'

rm $slotlist

done < $rellist
