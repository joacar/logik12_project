TESTS_FOLDER = tests/

all: test1 test2 test3 test4 test5

%:
	@ ./input.py -c $(TESTS_FOLDER)$*.txt

%.txt:
	@ ./input.py -c $(TESTS_FOLDER)$*.txt
	sicstus -l $*_engine.pl
	@ while [ ! -f solution.out ] ; do sleep 1; done;
	@ ./output.py -c $(TESTS_FOLDER)$*.txt solution.out

clean:
	rm -f *~ *_db.pl *_engine.pl *.out