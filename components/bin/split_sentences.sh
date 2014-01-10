#!/bin/bash

# This script reads document ids from a .dscore file and
# creates a file with all the sentences in these documents.
#
# Author: Grzegorz Chrupala and Benjamin Roth

# Alternative splitter using smt tools:
# MSCRIPTS=$TAC_ROOT/singlefile/components/scripts
# | $MSCRIPTS/split-sentences.perl -l en \
# | grep -v '^<P>$' \


DSCORE=$1
DRANK=$2
SEG_HOME=$TAC_ROOT/components/sentence_segmentation

COMP=$TAC_ROOT/components/pipeline
JAVA_HOME=$TAC_ROOT/lib/java/jdk1.6.0_18

export PERL5LIB="$SEG_HOME/src" # split.pl needs this

>$DRANK
for path_id in `cut -d' ' -f3 $DSCORE`
do
 INPUT=`echo $path_id | cut -f1 -d:`
 DOCID=`echo $path_id | cut -f2 -d:`

 $JAVA_HOME/bin/java -cp $COMP/dist/components.jar \
 run.ExtractText $INPUT $DOCID \
 | sed 's= \.\.\. @=...@=g' \
 | $SEG_HOME/bin/split.pl \
 | $SEG_HOME/bin/tokenize.sed \
 | $SEG_HOME/bin/Format $DOCID >> $DRANK
done

