writeSolution(L):-
	tell('solution.out'),
	printSolution(L),
	seen,
	told.

printSolution([]).
printSolution([H|T]):-
	printSolution(H),
	nl,
	printSolution(T).
printSolution([[R,C], [R1,C1], B]):-
	write(R), write(' '), write(C), write(' '),
	write(R1), write(' '),write(C1), write(' '),
	write(B).

generate(List, Solution):-
	generate(List, [], Solution).
	% verify solution: Check remaining constraints

generate([], C, C). 
generate([[I,Adjacent]|T], Acc, C):-
	connect(I, Adjacent, Acc, L),
	generate(T, L, C).

% connect(+Island, +AdjacentToIsland, +Acc[], -ConnectedIslands)
connect([C,B],[],L,L):-
	sumblist(C,L,R),
	sumlist(R, S),
	S == B.
connect(A, [B|T], Acc, L):-
	[Co1,Ab] = A, [Co2,Bb] = B,
	\+ connected(Co1,Co2,Acc),
	sumblist(Co1,Acc,Ar),
	sumlist(Ar,X1),
	sumblist(Co2,Acc,Br),
	sumlist(Br,Y1),
	X is Ab - X1,
	Y is Bb - Y1,
	X >= 0, Y >= 0,
	minb([Co1,X],[Co2,Y],MinB),
	bridges(MinB, BridgesE),
	select(Aa, BridgesE, _),
	(Aa == 0 -> 
		connect(A,T,Acc,L);
		connect(A,T, [[Co1,Co2,Aa]|Acc], L)
	).
connect([A,X], [[B,_]|T], Acc, L):-
	connected(A,B,Acc),
	connect([A,X],T,Acc,L).

connected(_,_,[]):-false.
connected(X,Y,[[X,Y,_]|_]).
connected(X,Y,[[Y,X,_]|_]). % Transitivity/undirected edges
connected(X,Y,[_|T]):-
	connected(X,Y,T).

%sumb(+Island,+Island,+Connected,-BridgesLeft, -BridgesLeft)
sumblist(_,[],[0]).
sumblist(C, [H|T], L):-
	sumblist(C, [H|T], [], L).

sumblist(_,[],[],[0]).
sumblist(_, [], L, L).
sumblist(C, [H|T], Acc, L):-
	[Cx, Cy, Bz] = H,
	C == Cx,
	C \= Cy,
	sumblist(C,T,[Bz|Acc], L).
sumblist(C, [H|T], Acc, L):-
	[Cx, Cy, Bz] = H,
	C == Cy,
	C \= Cx,
	sumblist(C,T,[Bz|Acc], L).
sumblist(C, [H|T], Acc, L):-
	[Cx, Cy, _] = H,
	C \= Cy,
	C \= Cx,
	sumblist(C,T,Acc, L).

% Constraint: At most two bridges connects two islands
minb([_,A], [_,B], R):-
	min(A,B,R), R =< 2.
minb([_,A], [_,B], 2):-
	min(A,B,R), R > 2.
min(A, B, A):- A =< B.
min(A, B, B):- A > B.
max(A,B,A):- A >= B.
max(A,B,B):- A < B.

bridges(N,L):-
	N >= 0, bridges(N,0,[],L).
bridges(N,N1,L,L):-N1 is N + 1, !.
bridges(N,N1,Acc,L):-
	N2 is N1 + 1,
	bridges(N,N2,[N1|Acc],L).

enumerate(1,[1]).
enumerate(2,[1,1]).
enumerate(2,[2]).
enumerate(3,[1,1,1]).
enumerate(3,[1,2]).
enumerate(4,[1,1,1,1]).
enumerate(4,[1,1,2]).
enumerate(4,[2,2]).
enumerate(5,[1,1,1,2]).
enumerate(5,[1,2,2]).
enumerate(6,[1,1,2,2]).
enumerate(6,[2,2,2]).
enumerate(7,[2,2,2,1]).
enumerate(8,[2,2,2,2]).

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
% Transforms a matrix graph into a list graph (adjacent list)
transform(Grid, AdjacentList) :-
	islands(Grid, Islands),
	transform(Islands, Islands, AdjacentList).

transform([], _, []).
transform([H|T], Islands, [[H|[Adjacent]]|Adjacencies]) :-
	[Coord,_] = H,
	directlyAdjacent(Coord,Islands,Adjacent),
	transform(T, Islands, Adjacencies), !.

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