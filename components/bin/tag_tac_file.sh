# tag_tac_file.sh <sgml_file.gz>
# This tags gzipped sgml files as provided by TAC.
# For a file of the form some-file.gz a file some-file.dtag.gz is created.
sgmlgz=$1
taggz="${sgmlgz%.*}".dtag.gz

SEG_HOME=$TAC_ROOT/components/sentence_segmentation
export PERL5LIB="$SEG_HOME/src" # split.pl needs this
UNWRAP=$TAC_ROOT/components/ner/bin/Unwrap
MODEL=$TAC_ROOT/components/ner/data/bbn-wsj-02-21.reduced2,r0.001,b10,i10.model
SPECIALENT=$TAC_ROOT/resources/special_NE_list/

zcat $sgmlgz \
| $TAC_ROOT/components/bin/run.sh run.ExtractAllText \
| sed 's= \.\.\. @=...@=g' \
| $SEG_HOME/bin/split2.pl \
| $SEG_HOME/bin/tokenize2.sed \
| $TAC_ROOT/components/bin/run.sh run.FormatSimple \
| $UNWRAP +RTS -K100m -H700m -A100m -RTS $MODEL \
 $SPECIALENT/JOB_TITLE \
 $SPECIALENT/NORP:RELIGION \
 $SPECIALENT/CAUSE_DEATH \
 $SPECIALENT/CHARGES \
| gzip > $taggz
