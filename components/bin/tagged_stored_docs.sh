#!/bin/bash
# This script retrieves tagged and gzipped documents.
#
# Author: Benjamin Roth

DRANK=$1
DTAG=$2
INDEX=`$TAC_ROOT/bin/get_config.sh index`

$TAC_ROOT/components/bin/run.sh run.TaggedStoredDocs $DRANK $INDEX > $DTAG 

