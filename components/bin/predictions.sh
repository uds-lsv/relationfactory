#!/bin/bash
FEATFILE=$1
PREDFILE=$2
RELFILE=`$TAC_ROOT/bin/get_expand_config.sh rellist $TAC_ROOT/config/rellist`
MODELPATH=`$TAC_ROOT/bin/get_expand_config.sh model`

echo
echo "Using relation models from:"
echo $MODELPATH

>$PREDFILE

while read RELATION
do
  echo $RELATION
  MODEL=$MODELPATH/$RELATION.mdl
  
  if [ ! -f $MODEL ]
  then
    echo "==="
    echo "WARNING"
    echo "no model found for relation: " $RELATION
    echo "... SKIPPING ..."
    echo "==="
    continue      # Skip rest of this particular loop iteration.
  fi

  TMPFILE=`mktemp`
  TMPFEATS=`mktemp`
  TMPBODY=`mktemp`

  cat $FEATFILE \
  | awk -v RELATION=$RELATION -F $'\t' '$2 == RELATION { print $0 }' \
  > $TMPFILE

  cat $TMPFILE | cut -f1-8 > $TMPBODY
  cat $TMPFILE | cut -f9 > $TMPFEATS

  # TODO: enter normal form in config file
  # Not necessary, from 2012 on, relation names can all be valid filenames.
  # NORMREL=`echo $RELATION | tr ':' '_' | tr '/' '_'`
  # MODEL=$MODELPATH/$NORMREL.mdl

  #/home/beroth/libs/svm_light/svm_classify $TMPFEATS $MODEL $TMPFILE
  $TAC_ROOT/lib/svm_light/svm_classify $TMPFEATS $MODEL $TMPFILE
  paste $TMPBODY $TMPFILE >> $PREDFILE

  rm $TMPFILE
  rm $TMPFEATS
  rm $TMPBODY
done < $RELFILE
