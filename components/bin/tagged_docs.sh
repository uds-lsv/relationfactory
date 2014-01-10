#!/bin/bash
# This script retrieves tagged and gzipped documents.
#
# Author: Benjamin Roth

DRANK=$1
DTAG=$2

$TAC_ROOT/components/bin/run.sh run.TaggedDocs $DRANK > $DTAG 

