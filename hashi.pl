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
	append([Top], [], Res1),
	adjacentBottom([R,C],Islands,Bottom),
	append([Bottom], Res1, Res2),
	adjacentLeft([R,C],Islands,Left),
	append([Left], Res2, Res3),
	adjacentRight([R,C],Islands,Right),
	append([Right],Res3, Res4), !,
	cleanEmpty(Res4,Res).
cleanEmpty([], []).
cleanEmpty([[]|T],Res):-
	cleanEmpty(T, Res).
cleanEmpty([H|T], [H|Res]):-
	cleanEmpty(T,Res).

allowedState([Row,Col]):- 
	Row > 0, 
	Col > 0, 
	rows(LastRow), Row =< LastRow,
	columns(LastCol), Col =< LastCol.

adjacentTop([Row,Col], _, []):-
	\+ allowedState([Row,Col]).
adjacentTop([Row,Col], Islands, [[RowUp,Col],B]):-
	RowUp is Row - 1,
	allowedState([RowUp, Col]),
	member([[RowUp,Col],B], Islands).
adjacentTop([Row,Col], Islands, Adj):-
	RowUp is Row - 1,
	adjacentTop([RowUp,Col], Islands, Adj).

adjacentBottom([Row,Col],_,[]):-
	\+ allowedState([Row,Col]).
adjacentBottom([Row,Col], Islands, [[RowUp,Col],B]):-
	RowUp is Row + 1,
	allowedState([RowUp, Col]),
	member([[RowUp,Col],B], Islands).
adjacentBottom([Row,Col], Islands, Adj):-
	RowUp is Row + 1,
	adjacentBottom([RowUp,Col], Islands, Adj).

adjacentLeft([Row,Col],_,[]):-
	\+ allowedState([Row,Col]).
adjacentLeft([Row, Col], Islands, [[Row, NewCol], B]):-
	NewCol is Col - 1,
	allowedState([Row, NewCol]),
	member([[Row,NewCol],B],Islands).
adjacentLeft([Row, Col], Islands, Adj):-
	NewCol is Col - 1,
	adjacentLeft([Row, NewCol],Islands,Adj).

adjacentRight([Row,Col],_,[]):-
	\+ allowedState([Row,Col]).
adjacentRight([Row, Col], Islands, [[Row, NewCol], B]):-
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