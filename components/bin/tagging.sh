#!/bin/bash 
# Tags sentences in drank format.
#
# Author: Grzegorz Chrupala and Benjamin Roth

set -e

SPECIALENT=$TAC_ROOT/resources/special_NE_list/

# Convert to pure CoNLL format
$TAC_ROOT/components/ner/bin/tac-conll to-conll $1 > $1.conll.tmp

# Run tagger
cat $1.conll.tmp | $TAC_ROOT/components/ner/lib/sequor/bin/seminer en > $1.sequor.tmp

# Add special entities
paste -d' ' $1.conll.tmp $1.sequor.tmp | $TAC_ROOT/components/ner/bin/specialents \
           $SPECIALENT/JOB_TITLE $SPECIALENT/NORP:RELIGION\
           $SPECIALENT/CAUSE_DEATH $SPECIALENT/CHARGES > $1.ent.tmp

# Convert back to TAC format
$TAC_ROOT/components/ner/bin/tac-conll from-conll $1 $1.ent.tmp > $2

# Clean up tmp files
rm -f $1.conll.tmp  $1.sequor.tmp $1.ent.tmp
