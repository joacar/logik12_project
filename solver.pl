:-consult('test1.pl').

solve :-
	islands(Islands),	/* Unify Islands to computed value in test?.kb */
	createAdjacentList(Islands, Adjacencies), /* Compute a list matrix */ 
	solve(Islands, Adjacencies, Solution),
	halt.

solve([], [], Solution) :-
	printSolution(Solution), 
	nl.

createAdjacentList([], Adjacencies).
/* Computes a list of the form */
/* [[ island, [neighbors]], ... ] where island in Islands and all neighbors in {Islands \ island} */
createAdjacentList(Islands, Adjacencies):-
	findAdjacent(Islands, Islands, Adjacent),
	append(Adjacent, Adjacencies, New).

findAdjacent([H|T], Islands, Adjacent) :-
	findAdjacent(H,Island, Adjacent).

findAdjacent([[Row,Col],Number], Islands, Adjacent) :-
	NewCol is Col + 1,
	NewIsland = [[Row,NewCol],Number],
	checkCoordinate(NewIsland, Islands).

checkCoordinate([H,_], [[H,_]|_]).
checkCoordinate([H,_], [[_,_]|T]):-
	checkCoordinate([H,_], T).

getCoordinates([],[]).
getCoordinates([[Coord,_]|T], [[Coord]|Coords]):-
	getCoordinates(T, Coords).

getIslands([], []).
getIslands([[Coords, B]|T], [[Coords,B]|Islands]):-
	integer(B),
	getIslands(T, Islands).
getIslands([_|T], Islands):-
	getIslands(T,Islands).