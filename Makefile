TESTS_FOLDER = tests/

%.txt:
	./input.py -c $(TESTS_FOLDER)$*.txt
	sicstus -l $*.pl

clean:
	rm -f *~ *.kb *.out