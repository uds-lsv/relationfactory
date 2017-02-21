#!/bin/bash
# merge_responses <query_expanded_xml> <response>*
# a response is written to stdout

# MergeResponses <query_expanded_xml> <teamid> <response>*
$TAC_ROOT/components/bin/run.sh run.MergeResponses $1 lsv ${@:2}
