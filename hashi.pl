findOne(S):-
	grid(G),
	islands(G,I),
	transform(G,L),
	generate(L,S),
	test(S,I).

findAll(Solutions):-
	grid(G),
	islands(G,I),
	transform(G,L),
	generate(L,S),
	setof(S,test(S,I), Solutions).

writeSolution(L):-
	tell('solution.out'),
	printSolution(L),
	seen,
	told.

test(Solution, Islands):-
	isconnected(Solution, Islands), % That ALL islands are connected
	nocross(Solution). % No bridges cross each other -> disjoint sets

/*
 Compute the range that every bridge span.
 If any bridge that spans horizontally shares
 a point with any bridge that spans vertically
 they intersect.
*/
nocross(Solution):-
	horizontalbridges(Solution, [], Hb),
	verticalbridges(Solution, [], Vb), !,
	disjoint(Hb,Vb).

disjoint([],_).
disjoint([H|T], L):-
	disjoint1(H,L),
	disjoint(T,L).

disjoint1([],_).
disjoint1([H|T], [H1|T1]):-
	\+ memberchk(H, H1),
	disjoint1(T,[H1|T1]).
disjoint1(_, _):-fail.

horizontalbridges([], Horizontal, Horizontal).
horizontalbridges([[[X,Y],[X,B],_]|T], Acc, Horizontal):-
	rangeasc(Y,B,Range),
	ziponeh(X,Range, Coords),
	horizontalbridges(T, [Coords|Acc], Horizontal).
horizontalbridges([_|T], Acc, Horizontal):-
	horizontalbridges(T,Acc,Horizontal).
verticalbridges([], Horizontal, Horizontal).
verticalbridges([[[X,Y],[A,Y],_]|T], Acc, Horizontal):-
	rangeasc(X,A,Range),
	ziponev(Y,Range, Coords),
	verticalbridges(T, [Coords|Acc], Horizontal).
verticalbridges([_|T], Acc, Horizontal):-
	verticalbridges(T,Acc,Horizontal).

ziponeh(_,[],[]).
ziponeh(A, [H|T], [[A,H]|R]):-
	ziponeh(A, T, R).
ziponev(_,[],[]).
ziponev(A, [H|T], [[H,A]|R]):-
	ziponev(A, T, R).
rangeasc(A,B,[]):-
	R is B-A, R == 1, !.
rangeasc(A,B,[A1|Range]):-
	A1 is A + 1,
	rangeasc(A1,B,Range).

rangedesc(A,B,Range):-
	rangedesc(A,B,[],Range).
rangedesc(A,B,Range,Range):-
	R is B-A, R == 1, !.
rangedesc(A,B,Acc,Range):-
	A1 is A + 1,
	rangedesc(A1, B, [A1|Acc], Range).

/*
 A graph is connected if every pair of nodes are connected.
 A naive approach is to create all possible pairs of nodes
 and check that there is a path between them.
*/
isconnected(_,[]).
isconnected(Solution,[[A,_]|T]):-
	isconnected(Solution, A, T),
	isconnected(Solution, T).
isconnected(_,_,[]).
isconnected(Solution, A, [[B,_]|T]):-
	path(A,B,Solution,_),
	isconnected(Solution,A,T).

path(Start,Finish,Graph,Visited):-
	path(Start, Finish, Graph, [], Visited).
path(Node,Node,_,Visited,Visited).
path(Start, Finish, Graph, Acc, Visited):-
	adjacent(Start,Graph, Adjacent),
	\+ memberchk(Adjacent, Acc),
	path(Adjacent, Finish, Graph, [Start|Acc], Visited).

adjacent(Start, Graph, Adjacent):-
	member([Start,Adjacent,_], Graph).
adjacent(Start, Graph, Adjacent):-
	member([Adjacent,Start,_], Graph).

/* GENERATOR
Given the adjacent list graph a probable
solution is generated
*/
generate(List, Solution):-
	generate(List, [], Solution).
generate([], C, C). 
generate([[I,Adjacent]|T], Acc, C):-
	connect(I, Adjacent, Acc, L),
	generate(T, L, C).

/*
If A and B are not connected,
check how many bridges are connected to A and B,
if there are room for a bridge between A and B,
select the minimum of A and B and produce possible
bridge configurations,
select the first option X and connect A and B with X
*/
% connect(+Island, +AdjacentToIsland, +Acc[], -ConnectedIslands)
connect([C,B],[],L,L):-
	sumblist(C,L,R),
	sumlist(R, S),
	S == B.
connect(A, [B|T], Connected, L):-
	[X,_] = A, [Y,_] = B,
	\+ connected(X,Y,Connected),	% Check A and B are NOT connected
	remainingBridges(A, Connected, RemainingA), % Calculate the remaining number of bridges 
	remainingBridges(B,Connected, RemainingB),
	minb(RemainingA,RemainingB,MinBridges),  % Heuristic. Could experiment with MAX
	bridges(MinBridges, Bridges), % Heuristic. Could experment with ASC and DESC
	select(Bridge, Bridges, _),
	(Bridge == 0 -> 
		connect(A,T,Connected,L);
		connect(A,T, [[X,Y,Bridge]|Connected], L)
	).
% If A and B are connected, move on
connect([A,X], [[B,_]|T], Acc, L):-
	connected(A,B,Acc),
	connect([A,X],T,Acc,L).

/*
Check if two islands are connected with a bridge. The connection is undirected
*/
connected(_,_,[]):-false.
connected(X,Y,[[X,Y,_]|_]).
connected(X,Y,[[Y,X,_]|_]). % Transitivity/undirected edges
connected(X,Y,[_|T]):-
	connected(X,Y,T).

remainingBridges([Coordinates,Bridges], Connected, Remaining):-
	sumblist(Coordinates,Connected,ListOfBridges),
	sumlist(ListOfBridges,SumConnected),
	Remaining is Bridges - SumConnected,
	Remaining >= 0.

/*
Takes a coordinate of an island along with a list of the connected islands
and computes the number of connected bridges it has
*/
% sumblist(+Coordinate, +Connected, -Sum of bridges)
sumblist(_,[],[0]):-!.
sumblist(C, [H|T], L):-
	sumblist(C, [H|T], [], L), !.

sumblist(_,[],[],[0]):-!.
sumblist(_, [], L, L):-!.
sumblist(C, [[C, _, B]|T], Acc, L):-
	sumblist(C,T,[B|Acc], L).
sumblist(C, [[_, C, B]|T], Acc, L):-
	sumblist(C,T,[B|Acc], L).
sumblist(C, [_|T], Acc, L):-
	sumblist(C,T,Acc, L).

/*
Computes the minimum island number from two islands,
taking the constraint 'At most two bridges connects two islands'
into account
*/
%minb(+Bridge,+Bridge,-Bridges)
minb(A, B, R):-
	min(A,B,R), R =< 2.
minb(A, B, 2):-
	min(A,B,R), R > 2.
min(A, B, A):- A =< B.
min(A, B, B):- A > B.

/*
Takes the island number and produces possible bridge connections
as a list, starting with the highest number
*/
%bridges(+Bridges, -Possible connections)
bridges(N,L):-
	N >= 0, bridges(N,0,[],L).
bridges(N,N1,L,L):-N1 is N + 1, !.
bridges(N,N1,Acc,L):-
	N2 is N1 + 1,
	bridges(N,N2,[N1|Acc],L).

select(X,[X|T],T).
select(X,[H|T1],[H|T2]):-
	select(X,T1,T2).

perm([],[]).
perm([H|T],P):-
	perm(T,R),
	select(H,P,R).

sumlist(L, X):-
	sumlist(L,0,X).
sumlist([],X,X).
sumlist([H|T], Acc, X):-
	Acc1 is Acc + H,
	sumlist(T,Acc1,X).
/*
Transforms a matrix graph into a list graph (adjacent list)
*/
% transform(+Grid, -Adjacent list)
transform(Grid, AdjacentList) :-
	islands(Grid, Islands),
	transform(Islands, Islands, AdjacentList).

transform([], _, []).
transform([H|T], Islands, [[H|[Adjacent]]|Adjacencies]) :-
	[Coord,_] = H,
	directlyAdjacent(Coord,Islands,Adjacent),
	transform(T, Islands, Adjacencies), !.

/*
Computes all the directly adjacent islands by
stepping north, east, south and west from the current island
and stops when an island occurs
*/
directlyAdjacent([R,C], Islands, Res):-
	adjacentTop([R,C], Islands, Top),
	append(Top, [], Res1),
	adjacentBottom([R,C],Islands,Bottom),
	append(Bottom, Res1, Res2),
	adjacentLeft([R,C],Islands,Left),
	append(Left, Res2, Res3),
	adjacentRight([R,C],Islands,Right),
	append(Right,Res3, Res), !. 

allowedState([Row,Col]):- 
	Row > 0, 
	Col > 0, 
	rows(LastRow), Row =< LastRow,
	columns(LastCol), Col =< LastCol.

adjacentTop([Row,Col], _, []):-
	\+ allowedState([Row,Col]).
adjacentTop([Row,Col], Islands, [[[RowUp,Col],B]]):-
	RowUp is Row - 1,
	allowedState([RowUp, Col]),
	member([[RowUp,Col],B], Islands).
adjacentTop([Row,Col], Islands, Adj):-
	RowUp is Row - 1,
	adjacentTop([RowUp,Col], Islands, Adj).

adjacentBottom([Row,Col],_,[]):-
	\+ allowedState([Row,Col]).
adjacentBottom([Row,Col], Islands, [[[RowUp,Col],B]]):-
	RowUp is Row + 1,
	allowedState([RowUp, Col]),
	member([[RowUp,Col],B], Islands).
adjacentBottom([Row,Col], Islands, Adj):-
	RowUp is Row + 1,
	adjacentBottom([RowUp,Col], Islands, Adj).

adjacentLeft([Row,Col],_,[]):-
	\+ allowedState([Row,Col]).
adjacentLeft([Row, Col], Islands, [[[Row, NewCol], B]]):-
	NewCol is Col - 1,
	allowedState([Row, NewCol]),
	member([[Row,NewCol],B],Islands).
adjacentLeft([Row, Col], Islands, Adj):-
	NewCol is Col - 1,
	adjacentLeft([Row, NewCol],Islands,Adj).

adjacentRight([Row,Col],_,[]):-
	\+ allowedState([Row,Col]).
adjacentRight([Row, Col], Islands, [[[Row, NewCol], B]]):-
	NewCol is Col + 1,
	allowedState([Row, NewCol]),
	member([[Row,NewCol],B],Islands).
adjacentRight([Row, Col], Islands, Adj):-
	NewCol is Col + 1,
	adjacentRight([Row, NewCol],Islands,Adj).

islands([], []).
islands([[Coords, B]|T], [[Coords,B]|Islands]):-
  integer(B),
  islands(T, Islands).
islands([[_, B]|T], Islands):-
  \+ integer(B),
  islands(T,Islands).

%%%%%%%%%%
% Debug  %
%%%%%%%%%%
printListMatrix([]).
printListMatrix([[H|Adjacent]|T]):-
	write(H), print(' is adjacent to '),
	printIslands(Adjacent),
	nl,
	printListMatrix(T).

printIslands([]).
printIslands([H|[]]):-
	write(H).
printIslands([H|T]):-
	T \= [],
	write(H), write(', '),
	printIslands(T).

printSolution([]).
printSolution([H|T]):-
	printSolution(H),
	nl,
	printSolution(T).
printSolution([[R,C], [R1,C1], B]):-
	write(R), write(' '), write(C), write(' '),
	write(R1), write(' '),write(C1), write(' '),
	write(B).

%%%%%%%%%%%%%%%%
% EXPERIMENTAL 
% NOT IN USE
%%%%%%%%%%%%%%%%
dfs(Node, Graph, Visited):-
	dfs(Node, Graph, [], Visited).

dfs(Node, Graph, Visited, V):-
	nextMoves(Node, Graph, [], Adjacent), !,
	subdfs(Adjacent, Graph, Visited, V),
	V = Visited.

subdfs([],_,Visited,Visited).
subdfs([],_,_,_).
subdfs([H|T], Graph, Visited, V):-
	\+ memberchk(H,Visited),
	dfs(H, Graph, [H|Visited], V),
	subdfs(T, Graph, Visited, V).
subdfs([H|T], Graph, Visited, V):-
	memberchk(H,Visited),
	subdfs(T,Graph,Visited,V).

filterNext([], _,[]).
filterNext([Next|T], Visited, [Next|NotVisited]):-
	\+ memberchk(Next, Visited),
	filterNext(T,Visited, NotVisited).
filterNext([Next|T], Visited, NotVisited):-
	memberchk(Next, Visited),
	filterNext(T,Visited, NotVisited).

nextMoves(_, [], Adjacent, Adjacent):-!.
nextMoves(Node, [[Node, Next, _]|T], Acc, Adjacent):-
	nextMoves(Node, T, [Next|Acc], Adjacent),!.
nextMoves(Node, [[Next, Node, _]|T], Acc, Adjacent):-
	nextMoves(Node, T, [Next|Acc], Adjacent),!.
nextMoves(Node, [_|T], Acc, Adjacent):-
	nextMoves(Node, T, Acc, Adjacent),!.