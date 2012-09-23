:-use_module(library(lists)).

% Load map
:-consult('test1.pl').

% Helpers

range(Low, Low, High) :- High >= Low.
range(Out,Low,High) :- NewLow is Low+1, NewLow =< High, range(Out, NewLow, High).

zip([], [], []).
zip([H1|T1], [H2|T2], [[H1, H2] | Rest]) :- zip(T1, T2, Rest).

repeat(_, 0, []).
repeat(Item, N, [Item | Rest]) :- N > 0, M is N-1, repeat(Item, M, Rest).

duplicate(L, D) :- append(L, L, D).

take(0, _, []).
take(N, L, [R1|Rest]) :- 
  select(R1, L, L2),
  N > 0, M is N-1,
  take(M, L2, Rest).

comb(0,_,[]).
comb(N, [X|T], [X|Rest]):- N > 0, M is N-1, comb(M, T, Rest).
comb(N, [_|T], Rest) :- N > 0, comb(N, T, Rest).

%% Map predicates

% Directional groups

northward([X,Y], [X,Y1]) :- Min is 0, Max is Y-1, range(Y1, Min, Max). 
eastward([X,Y], [X1,Y]) :- width(W), Min is X+1, Max is W-1, range(X1, Min, Max).
southward([X,Y], [X,Y1]) :- height(H), Min is Y+1, Max is H-1, range(Y1, Min, Max).
westward([X,Y], [X1,Y]) :- Min is 0, Max is X-1, range(X1, Min, Max). 

% Distances in the X and Y axes

distance([X,Y1], [X,Y2], D) :- Y2 =< Y1, D is Y1 - Y2.
distance([X,Y1], [X,Y2], D) :- Y2 > Y1, D is Y2 - Y1.

distance([X1,Y], [X2,Y], D) :- X1 =< X2, D is X1 - X2.
distance([X1,Y], [X2,Y], D) :- X1 > X2, D is X2 - X1. 

% Islands

northIsland(Coords, NorthCoords) :- northward(Coords, NorthCoords), island(NorthCoords).
eastIsland(Coords, EastCoords) :- eastward(Coords, EastCoords), island(EastCoords).
southIsland(Coords, SouthCoords) :- southward(Coords, SouthCoords), island(SouthCoords).
westIsland(Coords, WestCoords) :- westward(Coords, WestCoords), island(WestCoords).

northIslands(Coords, Islands) :- findall(IslandCoords, northIsland(Coords, IslandCoords), Islands).
closestNorthIsland(Coords, ClosestCoords) :- !, northIslands(Coords, Islands), last(Islands, ClosestCoords).

eastIslands(Coords, Islands) :- findall(IslandCoords, eastIsland(Coords, IslandCoords), Islands).
closestEastIsland(Coords, ClosestCoords) :- !, eastIslands(Coords, Islands), head(Islands, ClosestCoords).

southIslands(Coords, Islands) :- findall(IslandCoords, southIsland(Coords, IslandCoords), Islands).
closestSouthIsland(Coords, ClosestCoords) :- !, southIslands(Coords, Islands), head(Islands, ClosestCoords).

westIslands(Coords, Islands) :- findall(IslandCoords, westIsland(Coords, IslandCoords), Islands).
closestWestIsland(Coords, ClosestCoords) :- !, westIslands(Coords, Islands), last(Islands, ClosestCoords).

% Bridges

bridgeable(Start, End) :- 
  closestNorthIsland(Start, End); 
  closestEastIsland(Start, End); 
  closestSouthIsland(Start, End);
  closestWestIsland(Start, End).

possibleBridgesFrom(Start, Bridges) :- 
  findall(End, bridgeable(Start, End), EndPoints), 
  duplicate(EndPoints, DupEndPoints), % As we can have two bridges to the same endpoint
  bridgeLimit(Start, L),
  comb(L, DupEndPoints, Possibilities),
  length(Possibilities, P),
  repeat(Start, P, StartPoints),
  zip(StartPoints, Possibilities, Bridges).

% possibleBridgesFrom should also take a list of CURRENT BRIDGES into account, and only add consistent bridge-bindings to that list.
% This allows us to push some of the test into our generation, by not producing bindings that are inconsistent
% with ones that already exist. Consistency means that if there is a bridge from I1 to I2, then there should be a bridge 
% from I2 to I1, and we should only add as many more bridges as are required given the ones already assigned.
% If there are already too many bridges for one island, we should fail.

% A possible solution is then a list of bridges [StartIsland, EndIsland]: [[I1, I2], [I2, I1], [I1, I3] ...]

% The solutions needs to pass one more constraint: connected(Bridges), which should be straightforward given a list of bridges.

% PROBLEMS: | ?- possibleBridgesFrom([0,0], Bridges).
%Bridges = [[[0,0],[2,0]],[[0,0],[0,2]]] ? ; As we can see, the duplicate EndPoints are treated differently everywhere, leading to redundancy
%Bridges = [[[0,0],[2,0]],[[0,0],[2,0]]] ? ;
%Bridges = [[[0,0],[2,0]],[[0,0],[0,2]]] ? ;
%Bridges = [[[0,0],[0,2]],[[0,0],[2,0]]] ? ;
%Bridges = [[[0,0],[0,2]],[[0,0],[0,2]]] ? ;
%Bridges = [[[0,0],[2,0]],[[0,0],[0,2]]] ? ;
