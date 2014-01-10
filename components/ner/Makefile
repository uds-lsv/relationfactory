.PHONY: clean

all: lib/sequor bin/tac-conll bin/specialents

lib/0.4.2.tar.gz: 
	cd lib;\
        wget https://bitbucket.org/gchrupala/sequor/get/0.4.2.tar.gz

lib/sequor: lib/0.4.2.tar.gz
	cd lib;\
        tar zxf 0.4.2.tar.gz;\
        mv gchrupala-sequor-469faf10774f sequor/;\
        cd sequor;\
        cabal update;\
        cabal install --bindir=`pwd`/bin --datadir=`pwd`/share 

bin/tac-conll bin/specialents: src/specialents.hs src/tac-conll.hs ner.cabal
	cabal install --bindir=`pwd`/bin

clean:
	-rm -rf lib/sequor
	-rm -rf share
	-cabal clean
	-rm bin/tac-conll bin/specialents

