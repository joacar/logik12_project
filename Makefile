TESTS_FOLDER = tests/

%.txt:
	./input.py -c $(TESTS_FOLDER)$*.txt
	sicstus -l $*_engine.pl
	# wait until file write is completed ./output.py -c $(TESTS_FOLDER)$*.txt $*.out

clean:
	rm -f *~ *_db.pl *_engine.pl *.out