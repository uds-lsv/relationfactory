#!/bin/sh
# Response <query_expanded_xml> <prediction> <team_id>
# a response is written to stdout

$TAC_ROOT/components/bin/run.sh run.PredictionToResponse $1 $2 lsv > $3
