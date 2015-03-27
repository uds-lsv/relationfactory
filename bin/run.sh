#!/bin/bash
# run.sh <system.config> [<goal>]

config=$(cd "$(dirname "$1")"; pwd)/$(basename $1)
#config=`readlink -f $1`
#config=$1

export LANG=en_US.UTF-8

# If goal not given as argument, take goal from config file.
if [ $# -ne 2 ]
then
  goal=`TAC_CONFIG=$config $TAC_ROOT/bin/get_config.sh goal response`
else
  goal=$2
fi

# Take makefile and run-dir from config file.
makefile=`TAC_CONFIG=$config $TAC_ROOT/bin/get_expand_config.sh makefile`
rundir=`TAC_CONFIG=$config $TAC_ROOT/bin/get_expand_config.sh rundir .`

mkdir -p $rundir
cd $rundir
pwd

cp -v $makefile makefile
TAC_CONFIG=$config make $goal

cd -
