#!/bin/sh
# postprocess.sh <response> <query_xml> <response_postprocessed>
# This is the most simple form of post-processing. Use the year-specific versions
# e.g. postprocess2012.sh or postprocess2013.sh if applicable.
INPUT=$1
QUERYXML=$2
OUTPUT=$3

COMP=$TAC_ROOT/components/pipeline/
JAVA_HOME=$TAC_ROOT/lib/java/jdk1.6.0_18/

LINKSTAT=`$TAC_ROOT/bin/get_expand_config.sh wikilinks /dev/null`

# Remove redundancy according to anchor text heuristics.

$TAC_ROOT/components/bin/run.sh run.RedundancyEliminator $LINKSTAT $INPUT $QUERYXML \
> $OUTPUT

