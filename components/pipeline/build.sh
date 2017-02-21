#!/bin/sh
cd $TAC_ROOT/components/pipeline
#JAVA_HOME=$TAC_ROOT/lib/java/jdk1.6.0_18/
LOCALCLASSPATH=$JAVA_HOME/lib/tools.jar:./lib/antlib/*
ANT_HOME=./lib/antlib
echo "$JAVA_HOME/bin/java -Dant.home=$ANT_HOME -classpath $LOCALCLASSPATH org.apache.tools.ant.Main $*"
$JAVA_HOME/bin/java -Dant.home=$ANT_HOME -classpath $LOCALCLASSPATH org.apache.tools.ant.Main $*
