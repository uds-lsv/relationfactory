#!/bin/bash
# sfeatures.sh <sentences_in> <features_out>

SENTS=$1
FEATS=$2
FEATUREMAP=`$TAC_ROOT/bin/get_expand_config.sh featuremap`
FEATURESET=`$TAC_ROOT/bin/get_config.sh featureset`
#BROWNCLASSES=`$TAC_ROOT/bin/get_expand_config.sh brownclasses`
BROWNCLASSES=$TAC_ROOT/resources/relation_models/brown.classes
LOG4J=file://$TAC_ROOT/config/log4j.config

echo 'feature map: '$FEATUREMAP
echo 'feature set: '$FEATURESET
echo 'brown classes: '$BROWNCLASSES

#<feature_map> <brown_classes> <single=true|false> <update_map=true|false> <sentences> <features>
$TAC_ROOT/components/bin/run.sh run.Features $FEATUREMAP $BROWNCLASSES true false $SENTS $FEATS $FEATURESET
