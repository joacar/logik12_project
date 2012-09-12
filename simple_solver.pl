:-consult('simple.pl').

islands([], []).
islands([[Coords, B]|T], [[Coords,B]|Islands]):-
  integer(B),
  islands(T, Islands).
islands([[_, B]|T], Islands):-
  \+ integer(B),
  islands(T,Islands).

islandCoord(X) :- grid(G), islands(G,Islands), member([X,_], Islands).

coords([], []).
coords([[C1, _]|IS], [C1|CS]) :- coords(IS,CS).

%allowedX(X) :- X > 0, maxX(MX), X < MX.
%allowedY(Y) :- Y > 0, maxY(MY), Y < MY.

adjacentTo([X,Y], [AX,Y]) :- AX is X + 1.
adjacentTo([X,Y], [X,AY]) :- AY is Y + 1.
adjacentTo([X,Y], [AX,Y]) :- AX is X - 1, AX > 0.
adjacentTo([X,Y], [X,AY]) :- AY is Y - 1, AY > 0.

bridgeable(B) :- 
  islandCoord(A), islandCoord(C), adjacentTo(A,B), adjacentTo(C,B).