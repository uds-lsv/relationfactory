#!/bin/sh
# expand_query.sh <query_xml> <expanded_query_xml>
# Author: Benjamin Roth

RELLIST=`$TAC_ROOT/bin/get_expand_config.sh rellist $TAC_ROOT/config/rellist`
RELCONFIG=`$TAC_ROOT/bin/get_expand_config.sh relations.config $TAC_ROOT/config/relations.config`
LINKSTATS=$TAC_ROOT/resources/expansion/enwiki.linktext.counts
ORG_SUFFIXES=$TAC_ROOT/resources/expansion/org_suffixes

precision_expansion=`$TAC_ROOT/bin/get_config.sh precision_expansion true`

#Expand <query_xml> <relations> <relation_config> <expansions> <maxN> <expanded.xml>
$TAC_ROOT/components/bin/run.sh run.Expand $1 $RELLIST $RELCONFIG $LINKSTATS 10 $ORG_SUFFIXES $2 $precision_expansion
