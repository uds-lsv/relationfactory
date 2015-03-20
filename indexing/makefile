SHELL := /bin/bash

corpuspath=/projects/corpora/TAC_KBP_13/LDC2013E45_TAC_2013_KBP_Source_Corpus_disc_2
corpusdatapath=$(corpuspath)/data/English/
corpusdoclistspath=$(corpuspath)/docs/doclists/eng_{nw,wb,df}_doclist
indexname=LDC2013E45_TAC_2013

index: $(corpusdatapath)
	$$TAC_ROOT/components/bin/run.sh indexir.Indexing $< $(indexname) false $@

idfile_mapping: index
	$$TAC_ROOT/components/bin/run.sh indexir.IdFileMapping $< $@

isindexcomplete: idfile_mapping
	cut -f1 $< | sort > idfile_mapping.sorted && cat $(corpusdoclistspath) | sort > corpusdoclists.sorted &&\
	diffcount=$$(diff idfile_mapping.sorted corpusdoclists.sorted | wc -l | cut -d' ' -f1);\
	if [[ $$diffcount == 0 ]]; then echo "Yes. Yes, it is."; else echo "No! $$diffcount differences found."; fi
	rm idfile_mapping.sorted corpusdoclists.sorted
