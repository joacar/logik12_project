TESTS_FOLDER = tests/

%.txt:
	@ ./input.py -c $(TESTS_FOLDER)$*.txt
	sicstus -l $*_engine.pl
	@ while [ ! -f solution.out ] ; do sleep 1; done;
	@ ./output.py -c $(TESTS_FOLDER)$*.txt solution.out

clean:
	rm -f *~ *_db.pl *_engine.pl *.out