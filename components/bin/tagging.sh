#!/bin/bash 
# Tags sentences in drank format.
#
# Author: Grzegorz Chrupala and Benjamin Roth

echo $LANG

set -e

SPECIALENT=$TAC_ROOT/resources/special_NE_list/

# Convert to pure CoNLL format
echo "$TAC_ROOT/components/ner/bin/tac-conll to-conll $1 > $1.conll.tmp"
$TAC_ROOT/components/ner/bin/tac-conll to-conll $1 > $1.conll.tmp

echo "cat $1.conll.tmp | $TAC_ROOT/components/ner/lib/sequor/bin/seminer en > $1.sequor.tmp"
# Run tagger
cat $1.conll.tmp | $TAC_ROOT/components/ner/lib/sequor/bin/seminer en > $1.sequor.tmp

echo "Tagging special entities."
# Add special entities
paste -d' ' $1.conll.tmp $1.sequor.tmp \
| sed 's/^\ $//g' \
 > $1.pasted

$TAC_ROOT/components/ner/bin/tac-conll from-conll $1 $1.pasted > $1.pasted.dtag

cat $1.pasted.dtag \
| $TAC_ROOT/components/bin/run.sh run.SpecialEntities \
           $TAC_ROOT/components/ner/bin/specialents \
           $SPECIALENT/JOB_TITLE $SPECIALENT/NORP:RELIGION\
           $SPECIALENT/CAUSE_DEATH $SPECIALENT/CHARGES > $2

# Clean up tmp files
rm -f $1.conll.tmp  $1.sequor.tmp $1.pasted $1.pasted.dtag
