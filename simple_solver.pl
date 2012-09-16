:-consult('simple.pl').

islands([], []).
islands([[Coords, B]|T], [[Coords,B]|Islands]):-
  integer(B),
  islands(T, Islands).

islands([[_, B]|T], Islands):-
  \+ integer(B),
  islands(T,Islands).

coords([], []).
coords([[C1, _]|IS], [C1|CS]) :- 
	coords(IS,CS).

createAdjacencies(Grid, AdjacentList) :-
	islands(Grid, Islands),
	createAdjacencies(Islands, Islands, AdjacentList).

createAdjacencies([], _, []).
createAdjacencies([H|T], Islands, [[H|[Adjacent]]|Adjacencies]) :-
	findAdjacent(H, Islands, Adjacent),
	%filterDirectlyAdjacent(H, Adjacent, AdjacentF),
	createAdjacencies(T, Islands, Adjacencies), !.

findAdjacent(_, [], []).
findAdjacent(H,[H|T], Adjacent):-
	findAdjacent(H, T, Adjacent).
findAdjacent(H, [H1|T], [H1|Adjacent]):-
	isAdjacent(H,H1),
	findAdjacent(H, T, Adjacent).
findAdjacent(H, [H1|T], Adjacent):-
	\+ isAdjacent(H,H1),
	findAdjacent(H, T, Adjacent).

directlyAdjacentTo([R,C], []):-
	grid(G),
	coords(G, Coords),
	\+ member([R,C], Coords),
	!,
	fail.
directlyAdjacentTo([R,C], [R,C]):-
	grid(G),
	coords(G, Coords),
	member([R,C], Coords),
	islands(G, Islands),
	member([[R,C],_],Islands).
directlyAdjacentTo([R,C], [R,C]):-
	grid(G),
	coords(G, Coords),
	member([R,C], Coords),
	islands(G, Islands),
	\+member([[R,C],_],Islands),
	R1 is R + 1,
	directlyAdjacentTo([R1,C], []).

isAdjacent([[R,_],_], [[R,_],_]).
isAdjacent([[_,C],_], [[_,C],_]).

next([R,C], [R1,C]):- R1 is R + 1.
next([R,C], [R1,C]):- R1 is R - 1.
next([R,C], [R,C1]):- C1 is C + 1.
next([R,C], [R,C1]):- C1 is C - 1.
horizontal([_,C], [_,C1]):-C < C1.
horizontal([_,C], [_,C1]):- C > C1.
vertical([R,_], [R1,_]):- R < R1.
vertical([R,_], [R1,_]):- R > R1.

/*
for those adjacent, if there are two on the same row/column;
	check right/left and up/down filter out those with the greatest
	distance to the node
*/
% Filter out the adjacent list that are directly adjacent, i.e. closest
filterAdjacentDirect(_,[],[]).
filterAdjacentDirect(I, [H|T], [H|Result]):-
	rightOf(I,H),
	filterAdjacentDirect(I, T, Result).
filterAdjacentDirect(I, [H|T], Result):-
	\+ rightOf(I,H),
	filterAdjacentDirect(I, T, Result).

islandCoord(X) :- grid(G), islands(G,Islands), member([X,_], Islands).

%allowedX(X) :- X > 0, maxX(MX), X < MX.
%allowedY(Y) :- Y > 0, maxY(MY), Y < MY.

% directlyRightOf(+Island, +Islands, +Closest to Island in Islands)
directlyAdjacentTo(_, [[[R1,C1],B1]|[]], [[R1,C1],B1]).
directlyAdjacentTo([[_,C],_], [[[_,C1],_]|T], [[R,C2],B]):-
	D is C1 - C,
	member([[R,C2],B], T),
	D1 is C2 - C,
	D1 < D.
directlyAdjacentTo([[_,C],_], [[[_,C1],_]|T], [[R,C2],B]):-
	D is C - C1,
	member([[R,C2],B], T),
	D1 is C - C2,
	D1 > D.
directlyAdjacentTo([[R,_],_], [[[R1,_],_]|T], [[R2,C],B]):-
	D is R1 - R,
	member([[R2,C],B], T),
	D1 is R2 - R,
	D1 < D.
directlyAdjacentTo([[R,_],_], [[[R1,_],_]|T], [[R2,C],B]):-
	D is R1 - R,
	member([[R2,C],B], T),
	D1 is R2 - R,
	D1 > D.

adjacentTo([X,Y], [AX,Y]) :- AX is X + 1, AX =< 3.
adjacentTo([X,Y], [X,AY]) :- AY is Y + 1, AY =< 5.
adjacentTo([X,Y], [AX,Y]) :- AX is X - 1, AX > 0.
adjacentTo([X,Y], [X,AY]) :- AY is Y - 1, AY > 0.

bridgeable(B) :- 
  islandCoord(A), 
  islandCoord(C),
  A \= C,
  adjacentTo(A,B),
  adjacentTo(C,B).

adjacentIslands(Island, AdjacentIslands) :-
	adjacent(Island, AdjacentIslands).

adjacentRight([R,C], A):-
	grid(G), 
	islands(G,I),
	coords(G,Coords),
	CU is C + 1,
	right([R,CU], I, Coords, A).

right([R,C], Islands, Coords, I) :-
	member([R,C], Coords), % Change to C here and below gets only first. Now backtracking gives second so
	C2 is C + 1,
	right([R,C2], Islands, I).

right([R,C], Islands, I) :-
	%print('Coord = '), write(R), print(','), write(C), nl,
	islandWithCoordinate([R,C],Islands,I).

islandWithCoordinate([R,C], I):-
	grid(G),
	islands(G, Islands),
	islandWithCoordinate([R,C], I, Islands).

islandWithCoordinate(_,_,[]) :-
	fail.
islandWithCoordinate([R,C], [[[R,C],B]|_], [[R,C],B]).
islandWithCoordinate([R,C], [_|T], I) :-
	islandWithCoordinate([R,C], T, I).

printAdjacencies([]).
printAdjacencies([[H|Adjacent]|T]):-
	write(H), print(' is adjacent to '),
	printIslands(Adjacent),
	nl,
	printAdjacencies(T).

printIslands([]).
printIslands([H|[]]):-
	write(H).
printIslands([H|T]):-
	T \= [],
	write(H), write(', '),
	printIslands(T).
