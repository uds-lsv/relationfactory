#!/bin/sh

PATTERNS=$TAC_ROOT/resources/manual_annotation/induced_patterns2013.txt

query_expanded=$1
candidates=$2
response=$3

#PatternResponse <query_expanded_xml> <sentences> <patterns>
$TAC_ROOT/components/bin/run.sh run.WeightedPatternResponse $query_expanded $candidates $PATTERNS > $response
