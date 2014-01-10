#!/bin/bash
# Queries Wikipedia for slotfillers.
# Author: Benjamin Roth

QUERIES_XML=$1
slots_out=$2
config=$TAC_ROOT/config/system_wiki.config
run_wiki=run_wiki

mkdir -p $run_wiki
cp $QUERIES_XML $run_wiki/query.xml
cd $run_wiki
cp $TAC_ROOT/bin/makefile .
TAC_CONFIG=$config make response_fast
grep -v NIL response_fast > response_fast.nonil
paste <(cat response_fast.nonil | cut -f2) <(cat response_fast.nonil | cut -f1,5) > wiki_slots
cd -
cp $run_wiki/wiki_slots $slots_out

# TODO: optionally delete directory




