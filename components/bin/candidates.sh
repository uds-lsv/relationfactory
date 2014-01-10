#!/bin/sh
#<expanded_query> <dtag> <candidates>

RELCONFIG=`$TAC_ROOT/bin/get_expand_config.sh relations.config $TAC_ROOT/config/relations.config`

$TAC_ROOT/components/bin/run.sh run.Candidates $1 $RELCONFIG $2 > $3
