GHC=$(TAC_ROOT)/lib/ghc/bin/ghc

all: run Unwrap

run:
	$(GHC) --make -isrc/ner:lib/labeler -L../../lib -o bin/run lib/labeler/run.hs -O2

Unwrap:
	$(GHC) --make -O2 -ilib/labeler:src/ner -L../../lib -o bin/Unwrap src/Unwrap.hs

train:
	./bin/run train False True 0.001 10 10 100 data/eng.raw.c500.classes\
         data/empty data/bbn-wsj-02-21.reduced2 data/bbn-wsj-22.reduced2\
         data/bbn-wsj-02-21.reduced2,r0.001,b10,i10.model\
         +RTS -K100m -H1500m

.PHONY: clean
clean:
	-rm `find src -name '*.o'`
	-rm `find src -name '*.hi'`
	-rm `find lib/labeler -name '*.o'`
	-rm `find lib/labeler -name '*.hi'`
	-rm bin/run
	-rm bin/Unwrap
#        -rm bin/runST
