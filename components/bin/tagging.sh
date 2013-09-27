#!/bin/bash 

# This script reads document ids from a .dscore file and
# creates a file with all the sentences in these documents.
#
# Author: Grzegorz Chrupala and Benjamin Roth

UNWRAP=$TAC_ROOT/components/ner/bin/Unwrap
MODEL=$TAC_ROOT/components/ner/data/bbn-wsj-02-21.reduced2,r0.001,b10,i10.model
SPECIALENT=$TAC_ROOT/resources/special_NE_list/

cat $1 \
|$UNWRAP +RTS -K100m -H700m -A100m -RTS $MODEL\
 $SPECIALENT/JOB_TITLE $SPECIALENT/NORP:RELIGION\
 $SPECIALENT/CAUSE_DEATH $SPECIALENT/CHARGES > $2

## TODO BEWARE CAUTION
## This is a workaround. Needed b/c old version of Haskell can not handle UTF8.
## drank is utf8 encoded. We convert it into latin1, feed it to tagging module, and convert it back later.
## This is lossy, though.
#fromenc=utf8
#toenc=latin1
#tmpfilein=$1.$toenc.tmp
#tmpfileout=$2.$toenc.tmp
## -c enables silent dropping of chars.
#iconv -c -f $fromenc -t $toenc -o $tmpfilein $1
#
#cat $tmpfilein \
#|$UNWRAP +RTS -K100m -H700m -A100m -RTS $MODEL\
# $SPECIALENT/JOB_TITLE $SPECIALENT/NORP:RELIGION\
# $SPECIALENT/CAUSE_DEATH $SPECIALENT/CHARGES > $tmpfileout
#
#
## convert back, and correct tagging for brackets, B-I-O tags etc.
#iconv -c -f $toenc -t $fromenc $tmpfileout \
#| $TAC_ROOT/components/bin/run.sh run.TaggingCorrector > $2


