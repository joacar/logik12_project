TESTS_FOLDER = tests/

all: test1 test2 test3

test1: test1.txt

%.txt:
	./input.py -c $(TESTS_FOLDER)$*.txt

clean:
	rm -f *~ *.kb *.out