#!/bin/bash
FEATFILE=$1
PREDFILE=$2
MODELPATH=`$TAC_ROOT/bin/get_expand_config.sh model`

echo
echo "Using relation model from:"
echo $MODELPATH

>$PREDFILE

RELATION='per:employee_or_member_of'
TMPFILE=`mktemp`
TMPFEATS=`mktemp`
TMPBODY=`mktemp`

# merge the two relations
# TODO: actually one of the two should be enough
cat $FEATFILE \
| sed 's#per:member_of#per:employee_or_member_of#g' \
| sed 's#per:employee_of#per:employee_or_member_of#g' \
| awk -v RELATION=$RELATION -F $'\t' '$2 == RELATION { print $0 }' \
> $TMPFILE

cat $TMPFILE | cut -f1-8 > $TMPBODY
cat $TMPFILE | cut -f9 > $TMPFEATS

MODEL=$MODELPATH/$RELATION.mdl

$TAC_ROOT/lib/svm_light/svm_classify $TMPFEATS $MODEL $TMPFILE
paste $TMPBODY $TMPFILE >> $PREDFILE

cp $PREDFILE $TMPFILE

# Disambiguate the merged relation again.
$TAC_ROOT/components/bin/disambiguate_employee_member.sh $TMPFILE $PREDFILE

rm $TMPFILE
rm $TMPFEATS
rm $TMPBODY
