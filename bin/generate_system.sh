#!/bin/sh

$TAC_ROOT/components/pipeline/build.sh

echo "compiling ner module"
cd $TAC_ROOT/components/ner/
cabal update
make
cd -

echo
echo "compiling classifier"
mkdir -p $TAC_ROOT/lib/svm_light
cd $TAC_ROOT/lib/svm_light
wget http://download.joachims.org/svm_light/current/svm_light.tar.gz
tar xzf svm_light.tar.gz
make
cd -



