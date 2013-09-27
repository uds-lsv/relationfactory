#!/bin/sh

echo
if [ -d "$TAC_ROOT/lib/ghc/bin" ]; then
  echo "haskell installed"
else
  echo "setting up haskell"
  $TAC_ROOT/lib/ghc/build.sh
fi

echo "compiling ner module"
cd $TAC_ROOT/components/ner/
make

