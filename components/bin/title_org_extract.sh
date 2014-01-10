#!/bin/sh

PATTERNS=$TAC_ROOT/resources/manual_annotation/title_org_patterns

in=$1
out=$2

$TAC_ROOT/components/bin/run.sh \
run.TitleOrgExtractor $PATTERNS < $in > $out
