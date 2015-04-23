#!/bin/sh

PATTERNS=`$TAC_ROOT/bin/get_expand_config.sh shortened_patterns`

query_expanded=$1
candidates=$2
response=$3

#PatternResponse <query_expanded_xml> <sentences> <patterns>
$TAC_ROOT/components/bin/run.sh run.WeightedPatternResponse $query_expanded $candidates $PATTERNS 0.0 true > $response
