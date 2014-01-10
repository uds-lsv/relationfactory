#!/bin/bash
hostname=$(hostname)
echo "Parser running on $hostname"
numcores=1
if [[ $hostname =~ cl[456]lx ]]; then numcores=4; fi
if [[ $hostname =~ ws[0-9]+lx ]]; then numcores=2; fi
numcores=`$TAC_ROOT/bin/get_config.sh numcores $numcores`
maxlen=`$TAC_ROOT/bin/get_config.sh maxsentlen 50`

mem=$((numcores*2 + 8))
echo "Using up to $mem Gigs RAM, running $numcores threads."
echo "Reading from $1, writing to $2. TAC_ROOT is $TAC_ROOT."
echo "Maximum sentence length $maxlen"

$TAC_ROOT/components/bin/run.sh -Xmx${mem}G run.ParseSentencesParallel $1 $TAC_ROOT/resources/parser_stanford/042013/englishPCFG.ser.gz $numcores $maxlen > $2

