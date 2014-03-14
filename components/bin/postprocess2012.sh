#!/bin/sh
# postprocess.sh <response> <query_xml> <response_postprocessed>
INPUT=$1
QUERYXML=$2
OUTPUT=$3

MAPPING=`$TAC_ROOT/bin/get_expand_config.sh idfile_mapping`

LINKSTAT=`$TAC_ROOT/bin/get_expand_config.sh wikilinks /dev/null`

# Bring date into TIMEX2 format if possible.
correct_date=`mktemp` 
$TAC_ROOT/components/bin/run.sh run.DateNormalizer $INPUT $correct_date

# Remove redundancy according to anchor text heuristics.
# Remove sentence number (i.e. part of sentence id after last dot).
# Heuristically match offsets.
$TAC_ROOT/components/bin/run.sh run.RedundancyEliminator $LINKSTAT $correct_date $QUERYXML \
| sed 's#^\([^\t]*\t[^\t]*\t[^\t]*\t\)\([^\t:]*\)\(:[^\t]\+\|$\)\(\t.*\)\?#\1\2\4#g' \
| python $TAC_ROOT/components/sentence_segmentation/src/DocMatch.py $MAPPING 2012 \
> $OUTPUT

rm $correct_date
