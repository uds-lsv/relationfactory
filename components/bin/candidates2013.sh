#!/bin/sh
# <expanded_query> <dtag> <dscore> <candidates>
# New candidate generation from 2013 onwards: Restores the original document offsets for sentences
# and args and encodes them in the document id.
expanded_query=$1
dtag=$2
dscore=$3
candidates=$4

RELCONFIG=`$TAC_ROOT/bin/get_expand_config.sh relations.config $TAC_ROOT/config/relations.config`
COREF=`$TAC_ROOT/bin/get_config.sh coref false`

CANDIDATES_CMD="$TAC_ROOT/components/bin/run.sh run.Candidates $expanded_query $RELCONFIG $dtag"

if [ "$COREF" = "true" ]; then
	echo "Using coref"
	CANDIDATES_CMD="$TAC_ROOT/components/bin/run.sh experimental.coref.CorefTagger $expanded_query $RELCONFIG $dtag"
fi

$CANDIDATES_CMD > $candidates.tmp && \
cat $candidates.tmp | $TAC_ROOT/components/bin/run.sh run.MatchRealOffsets $dscore \
> $candidates
