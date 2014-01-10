#!/bin/sh

echo
if [ -d "$TAC_ROOT/lib/java/jdk1.6.0_18" ]; then
  echo "java exists";
else
  echo "setting up java"
  cp $TAC_ROOT/lib/jdk1.6.0_18.tgz $TAC_ROOT/lib/java
  cd $TAC_ROOT/lib/java
  tar xzvf jdk1.6.0_18.tgz
fi

echo
if [ -d "$TAC_ROOT/lib/ghc/bin" ]; then
  echo "haskell installed"
else
  echo "setting up haskell"
  $TAC_ROOT/lib/ghc/build.sh
fi

echo
echo "compiling java based pipeline components"
$TAC_ROOT/components/pipeline/build.sh

echo
echo "compiling sentence segmentation module"
cd $TAC_ROOT/components/sentence_segmentation/
make
echo "compiling ner module"
cd $TAC_ROOT/components/ner/
make
#echo "compiling relations module"
#cd $TAC_ROOT/components/relations/
#make

# 2011 index: /projects/beroth/TAC/index
# 2012 index, including Chinese: /cl6local/TAC_CORPORA/index
# 2012 index, english only: /cl6local/TAC_CORPORA/eng_index

echo
echo "extracting relation models"
cd $TAC_ROOT/resources/relation_models
tar xzf mdls.tar.gz
cd -

echo
echo "compiling classifier"
cd $TAC_ROOT/lib/svm_light
tar xzf svm_light.tar.gz
make
cd -

echo
echo "compiling eval script"
cd $TAC_ROOT/evaluation/bin/
javac SFScore.java 
cd -


