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

comb(0,_,[]).
comb(N, [X|T], [X|Rest]):- N > 0, M is N-1, comb(M, T, Rest).
comb(N, [_|T], Rest) :- N > 0, comb(N, T, Rest).

difference(Xs,Ys,D) :- 
  findall(X,(member(X,Xs),\+(member(X,Ys))),D).

%% Map predicates

% Directional groups

northward([X,Y], [X,Y1]) :- Min is 0, Max is Y-1, range(Y1, Min, Max). 
eastward([X,Y], [X1,Y]) :- width(W), Min is X+1, Max is W-1, range(X1, Min, Max).
southward([X,Y], [X,Y1]) :- height(H), Min is Y+1, Max is H-1, range(Y1, Min, Max).
westward([X,Y], [X1,Y]) :- Min is 0, Max is X-1, range(X1, Min, Max). 

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

notTooManyTrailingBridgesFor(Island, StartIslands) :-
  length(StartIslands, S),
  bridgeLimit(Island, L),
  S =< L.

buildBridgesBetween(Start, EndPoints, Bridges) :-
  length(EndPoints, L),
  repeat(Start, L, StartPoints),
  zip(StartPoints, EndPoints, Bridges).

% Given some bridges and an island, ensure that 
% every bridge TO the island has a corresponding
% bridge FROM that island. Fail if the bridge has
% reached its limit.
connectTrailingBridges(BridgesIn, Island, BridgesOut) :-
  findall(Start, member([Start, Island], BridgesIn), StartIslands),
  notTooManyTrailingBridgesFor(Island, StartIslands),
  buildBridgesBetween(Island, StartIslands, NewBridges),
  append(BridgesIn, NewBridges, BridgesOut).

bridgesLeftToBuild(BridgesBuilt, Island, Left) :-
  findall(Start, member([Start, Island], BridgesBuilt), BridgesToMe),
  length(BridgesToMe, B),
  bridgeLimit(Island, Limit),
  Left is Limit - B.

buildNewBridges(BridgesIn, Island, NumBridges, BridgesOut) :-
  findall(Start, member([Start, Island], BridgesIn), BridgesToMe),
  findall(End, bridgeable(Island, End), EndPoints), 
  difference(EndPoints, BridgesToMe, Targets),      % Only build to Islands that are not already connected to me
  duplicate(Targets, DupTargets),                   % As we can have two bridges to the same target
  comb(NumBridges, DupTargets, Possibilities),      % <-- Do this as a reduction with a set to preserve uniqueness?
  buildBridgesBetween(Island, Possibilities, NewBridges),
  append(BridgesIn, NewBridges, BridgesOut).

%% Solution

% Reducing relation

accumulateBridges(BridgesIn, Island, BridgesOut) :-
  connectTrailingBridges(BridgesIn, Island, CompleteBridges),
  bridgesLeftToBuild(CompleteBridges, Island, ToMake),
  buildNewBridges(CompleteBridges, Island, ToMake, BridgesOut).

% Reduction

generate(Solution, [], Solution).
generate(BridgesIn, [Island | Rest], BridgesOut) :-
  accumulateBridges(BridgesIn, Island, Accumulated),
  generate(Accumulated, Rest, BridgesOut).
generate(Solution) :-
  findall(I, island(I), Islands),
  generate([], Islands, Solution).

% Use SETS: http://www.sics.se/sicstus/docs/4.0.5/html/sicstus/lib_002dordsets.html#lib_002dordsets

% Add global constraints:
%  (1) isConnected(Solution)
%  (2) noCrossing(Solution)
%  (3) Go through the Islands in Most Constrained Order 
