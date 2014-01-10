# tag_tac.sh <directory>
# This tags all gzipped sgml files as provided by TAC.
# For each file of the form some-file.gz a file some-file.dtag.gz is created.
directory=$1

SEG_HOME=$TAC_ROOT/components/sentence_segmentation
export PERL5LIB="$SEG_HOME/src" # split.pl needs this
UNWRAP=$TAC_ROOT/components/ner/bin/Unwrap
MODEL=$TAC_ROOT/components/ner/data/bbn-wsj-02-21.reduced2,r0.001,b10,i10.model
SPECIALENT=$TAC_ROOT/resources/special_NE_list/

find $directory -type f -name \*.gz \
| grep -v '.dtag.gz$' \
| while read fgz
do echo $fgz
 fdtaggz="${fgz%.*}".dtag.gz

 zcat $fgz \
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
 | gzip > $fdtaggz
done
