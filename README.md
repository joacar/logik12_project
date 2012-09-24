logik12_project
===============

Logic programming with Prolog for the course ID2213. Solving Hashiwokakero, a Japanese puzzle about connecting bridges between islands

Authors
---
Pascal Chatterjee and Joakim Carselind

Technical description
---
1. Place test files in the folder tests/ (see existing files for formatting)
2. Run $makefile testfile.txt
3. Run $./output.py -c testfile.txt solution.out

Files
---
* hashi.pl: Contains the logic to solve Hashiwokakero
* input.py: Produces the knowledge base file containing the dimension of the grid and the grid as a predicate. Also produces the engine prolog file that consults the knowledge base for facts about the puzzle and the hashi.pl and then executes the solve predicate and writes the solution to solution.out
* output.py: Takes the original test file and the prolog solution and prints a nice formatted solution with the connected islands
Makefile: To generate and execute tests
tests/: Test scenarios
solutions/: Solutions to the test scenarios
report/: Report and presentation for seminar