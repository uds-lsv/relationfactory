#!/bin/bash

# install haskell
LOCALGHC=$TAC_ROOT/lib/ghc
#mkdir $LOCALGHC
cd $LOCALGHC
tar xjvf ghc-6.10.4-x86_64-unknown-linux-n.tar.bz2
cd ghc-6.10.4/
./configure --prefix $LOCALGHC
make install
export PATH=$LOCALGHC/bin:$PATH
cd ..

# install haskell libraries
tar xzvf utf8-string-0.3.6.tar.gz
cd utf8-string-0.3.6
../bin/runhaskell Setup configure --prefix=$LOCALGHC
../bin/runhaskell Setup build
../bin/runhaskell Setup install
cd ..

tar xzvf binary-0.5.0.2.tar.gz
cd binary-0.5.0.2
../bin/runhaskell Setup configure --prefix=$LOCALGHC
../bin/runhaskell Setup build
../bin/runhaskell Setup install
cd ..

tar xzvf primitive-0.3.1.tar.gz
cd primitive-0.3.1
../bin/runhaskell Setup configure --prefix=$LOCALGHC
../bin/runhaskell Setup build
../bin/runhaskell Setup install
cd ..

tar xzvf vector-0.7.tar.gz
cd vector-0.7
../bin/runhaskell Setup configure --prefix=$LOCALGHC
../bin/runhaskell Setup build
../bin/runhaskell Setup install
cd ..

tar xzvf tagsoup-0.11.1.tar.gz
cd tagsoup-0.11.1
../bin/runhaskell Setup configure --prefix=$LOCALGHC
../bin/runhaskell Setup build
../bin/runhaskell Setup install
cd ..

