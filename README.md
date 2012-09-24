logik12_project
===============

Authors
---
Pascal Chatterjee and Joakim Carselind

Description
---
Logic programming with Prolog for the course ID2213. Solving Hashiwokakero, a Japanese puzzle about connecting islands using bridges according to the constraints

* They must begin and end at distinct islands, travelling a straight line in between.
* They must not cross any other bridges or islands.
* They may only run perpendicularly.
* At most two bridges connect a pair of islands.
* The number of bridges connected to each island must match the number on that island.
* The bridges must connect the islands into a single connected group.

Usage
---
1. Place test files in the folder tests/ (see existing files for formatting)
2. Run $make testfile.txt
3. Run $make clean

File information
---
* hashi.pl: 	Contains the logic to solve Hashiwokakero
* input.py: 	Produces the knowledge base file containing the dimension of the grid and 				  the grid as a predicate. Also produces the engine prolog file that 						consults the knowledge base for facts about the puzzle and the hashi.pl 				and then executes the solve predicate and writes the solution to solution.				  out
* output.py: 	Takes the original test file and the prolog solution and prints a nice 					formatted solution with the connected islands
Makefile: 		To generate and execute tests and clean
tests/: 		input files .txt
solutions/: 	output files. out
report/: 		Report and presentation for seminar