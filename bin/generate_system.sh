#!/bin/sh

$TAC_ROOT/components/pipeline/build.sh

#echo
#echo "compiling sentence segmentation module"
#cd $TAC_ROOT/components/sentence_segmentation/
#make
#echo "compiling ner module"
#cd $TAC_ROOT/components/ner/
#make


#echo
#if [ -d "$TAC_ROOT/lib/ghc/bin" ]; then
#echo "haskell installed"
#else
#echo "setting up haskell"
#  $TAC_ROOT/lib/ghc/build.sh
#fi

echo "compiling ner module"
cd $TAC_ROOT/components/ner/
make



#echo
#echo "compiling classifier"
#cd $TAC_ROOT/lib/svm_light
#tar xzf svm_light.tar.gz
#make
#cd -



