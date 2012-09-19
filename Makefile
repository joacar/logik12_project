TESTS_FOLDER = tests/

%.txt:
	./input.py -c $(TESTS_FOLDER)$*.txt
	sicstus -l $*_engine.pl

clean:
	rm -f *~ *_db.pl *_engine.pl *.out