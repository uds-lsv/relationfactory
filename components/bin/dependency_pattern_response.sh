#!/bin/sh

PATTERNS=$TAC_ROOT/resources/pris_patterns/pattern_pris.tsv

query_expanded=$1
candidates=$2
response=$3

#DependencyPatternResponse <query_expanded_xml> <sentences> <patterns> <team_id>
$TAC_ROOT/components/bin/run.sh \
run.DependencyPatternResponse $query_expanded $candidates $PATTERNS lsv > $response
