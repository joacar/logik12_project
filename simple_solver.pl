:-consult('simple.pl').

coords([], []).
coords([[C1, _]|IS], [C1|CS]) :- coords(IS,CS).

isBridgeable([[2,3],
              [3,2],
              [3,4]]).

% TODO Generate this from python.

allowedX(X) :- X > 0, X < 5.
allowedY(Y) :- Y > 0, Y < 5.

adjacentTo([X,Y], [AX,Y]) :- AX is X + 1, allowed(AX).
adjacentTo([X,Y], [X,AY]) :- AY is Y + 1.
adjacentTo([X,Y], [AX,Y]) :- AX is X - 1, AX > 0.
adjacentTo([X,Y], [X,AY]) :- AY is Y - 1, AY > 0.