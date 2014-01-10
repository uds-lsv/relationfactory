#!/bin/sh
# postprocess.sh <response> <query_xml> <title_org_map> <response_postprocessed>
INPUT=$1
QUERYXML=$2
ORG_MAP=$3
OUTPUT=$4

LINKSTAT=/dev/null

# Bring date into TIMEX2 format if possible.
correct_date=`mktemp` 
$TAC_ROOT/components/bin/run.sh run.DateNormalizer $INPUT $correct_date

# Remove redundancy according to anchor text heuristics.
# Convert formats.
$TAC_ROOT/components/bin/run.sh run.RedundancyEliminator $LINKSTAT $correct_date $QUERYXML $ORG_MAP \
| $TAC_ROOT/components/bin/run.sh run.RemoveSlots $QUERYXML $TAC_ROOT/resources/manual_annotation/disallowed_slots \
| $TAC_ROOT/components/bin/run.sh run.ConvertResponse2012To2013 \
> $OUTPUT

rm $correct_date

