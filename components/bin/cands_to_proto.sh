#!/bin/sh
#<candidates> <candidates.pb> [<weights>]

$TAC_ROOT/components/bin/run.sh run.CandidatesToProto $1 $2 $3
