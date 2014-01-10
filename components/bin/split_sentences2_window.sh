#!/bin/bash

# This script reads document ids from a .dscore file and
# creates a file with those sentences in the documents
# that have a query match.
#
# Author: Grzegorz Chrupala and Benjamin Roth
# Alternative splitter using smt tools:
# MSCRIPTS=$TAC_ROOT/singlefile/components/scripts
# | $MSCRIPTS/split-sentences.perl -l en \
# | grep -v '^<P>$' \


QUERY_EXPANDED=$1
DSCORE=$2
DRANK=$3
SEG_HOME=$TAC_ROOT/components/sentence_segmentation

export PERL5LIB="$SEG_HOME/src" # split.pl needs this

# Benchmarks (1000 docs):
# ExtractText2 + split2 + tokenize2 + Format: real 0m26.139s
# ExtractText2 + split2 + tokenize2: real    0m30.525s
# ExtractText2 + split2: real    0m30.161s
# ExtractText2: 0m30.609s


$TAC_ROOT/components/bin/run.sh run.ExtractText2 $DSCORE \
| sed 's= \.\.\. @=...@=g' \
| $SEG_HOME/bin/split2.pl \
| $SEG_HOME/bin/tokenize2.sed \
| $TAC_ROOT/components/bin/run.sh run.Format $QUERY_EXPANDED 1 \
> $DRANK

