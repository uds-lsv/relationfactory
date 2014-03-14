#!/bin/sh
# Creates a response from matching a list of tuples against candidates.
query_expanded=$1
kb_slots=$2
link_stats=`$TAC_ROOT/bin/get_expand_config.sh wikilinks /dev/null`
candidates=$3
response=$4

#run.ListMatchResponse <query_expanded_xml> <kb_slots> <link_stats> <candidates>
$TAC_ROOT/components/bin/run.sh run.ListMatchResponse $query_expanded $kb_slots $link_stats $candidates \
> $response
