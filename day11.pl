:- dynamic(location/3).

:- initialization(main, main).

main :-
  part_1(Part1),
  format("Part 1: ~d~n", Part1),
  part_2(Part2),
  format("Part 2: ~d~n", Part2),
  run_tests.

:- begin_tests(day11).
test(part_1) :- part_1(2346).
test(part_2) :- part_2(2111).
:- end_tests(day11).

% Assert facts in the form layout(X, Y, State, Iteration):
%   layout(0, 0, empty_seat, 0).
%   layout(1, 0, empty_seat, 0).
%   layout(2, 0, floor, _).
setup :-
  retractall(layout(_, _, _, _)),
  read_puzzle_input("puzzle_inputs/day11.txt", LayoutMap),
  findall(StartingState, starting_state(LayoutMap, StartingState), StartingStates),
  maplist(assert_layout, StartingStates).

read_puzzle_input(Filename, CharGrid) :-
  read_file_to_string(Filename, Input, []),
  split_string(Input, "\n", "\n", Lines),
  maplist(string_chars, Lines, CharGrid).

starting_state(LayoutMap, {X, Y, State, Iteration}) :-
  nth0(Y, LayoutMap, Row),
  nth0(X, Row, Char),
  state_char(State, Char),
  (State == floor, Iteration = _ ; Iteration = 0).

state_char(empty_seat, 'L').
state_char(occupied_seat, '#').
state_char(floor, '.').

assert_layout({X, Y, State, Iteration}) :-
  assert(layout(X, Y, State, Iteration)).

layout({X, Y}, State, Iteration) :-
  layout(X, Y, State, Iteration).

%%%%%%%%%%%%%%%%%%%%%%%

iterate_layout(Iteration, Config) :-
  foreach(next_state(Iteration, NextState, Config), assert_layout(NextState)), !.

next_state(Iteration, NextState, Config) :-
  PrevIteration is Iteration - 1,
  layout(X, Y, State, PrevIteration),
  next_state(X, Y, State, Iteration, NextState, Config).

next_state(_X, _Y, floor, _Iteration, _NextState, _Config) :-
  false.

next_state(X, Y, empty_seat, Iteration, NextState, {NeighborDefinition, _}) :-
  PrevIteration is Iteration - 1,
  num_occupied_neighbors({X, Y}, PrevIteration, NeighborDefinition, NumOccupiedNeighbors),
  (NumOccupiedNeighbors == 0 ->
    SeatState = occupied_seat
  ; true ->
    SeatState = empty_seat
  ),
  NextState = {X, Y, SeatState, Iteration}.

next_state(X, Y, occupied_seat, Iteration, NextState, {NeighborDefinition, TooManyNeighbors}) :-
  PrevIteration is Iteration - 1,
  num_occupied_neighbors({X, Y}, PrevIteration, NeighborDefinition, NumOccupiedNeighbors),
  (NumOccupiedNeighbors >= TooManyNeighbors ->
    SeatState = empty_seat
  ; true ->
    SeatState = occupied_seat
  ),
  NextState = {X, Y, SeatState, Iteration}.

num_occupied_neighbors(Pos, Iteration, NeighborDefinition, N) :-
  findall(Neighbor, call(NeighborDefinition, Pos, Neighbor), Neighbors),
  include(occupied_seat_pos(Iteration), Neighbors, OccupiedSeats),
  length(OccupiedSeats, N).

occupied_seat_pos(Iteration, {X, Y}) :-
  layout(X, Y, occupied_seat, Iteration).

stable_layout(Iteration, StableIteration, Config) :-
  NextIteration is Iteration + 1,
  iterate_layout(NextIteration, Config),
  (layout_change(Iteration, NextIteration) ->
    stable_layout(NextIteration, StableIteration, Config)
  ; true ->
    StableIteration = Iteration
  ).

layout_change(IterationA, IterationB) :-
  layout(X, Y, StateA, IterationA),
  layout(X, Y, StateB, IterationB),
  StateA \== StateB.

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
  setup,
  NeighborDefinition = adjacent_neighbor,
  TooManyNeighbors = 4,
  stable_layout(0, StableIteration, {NeighborDefinition, TooManyNeighbors}),
  findall({X, Y}, layout(X, Y, occupied_seat, StableIteration), OccupiedSeats),
  length(OccupiedSeats, Solution).

adjacent_neighbor({X, Y}, {NX, NY}) :-
  XLow is X - 1, XHigh is X + 1,
  YLow is Y - 1, YHigh is Y + 1,
  between(XLow, XHigh, NX),
  between(YLow, YHigh, NY),
  {NX, NY} \= {X, Y}.

%%%%%%%%%%%%%%%%%%%%%%%

part_2(Solution) :-
  setup,
  NeighborDefinition = visible_neighbor,
  TooManyNeighbors = 5,
  stable_layout(0, StableIteration, {NeighborDefinition, TooManyNeighbors}),
  findall({X, Y}, layout(X, Y, occupied_seat, StableIteration), OccupiedSeats),
  length(OccupiedSeats, Solution).

visible_neighbor(Pos, NeighborPos) :-
  visible_neighbor(Pos, n, NeighborPos) ;
  visible_neighbor(Pos, s, NeighborPos) ;
  visible_neighbor(Pos, e, NeighborPos) ;
  visible_neighbor(Pos, w, NeighborPos) ;
  visible_neighbor(Pos, ne, NeighborPos) ;
  visible_neighbor(Pos, se, NeighborPos) ;
  visible_neighbor(Pos, nw, NeighborPos) ;
  visible_neighbor(Pos, sw, NeighborPos).

visible_neighbor(Pos, Direction, NeighborPos) :-
  adjacent_pos(Pos, Direction, AdjacentPos),
  layout(AdjacentPos, State, _),
  (State == floor ->
    visible_neighbor(AdjacentPos, Direction, NeighborPos)
  ; true ->
    NeighborPos = AdjacentPos
  ),
  !.

adjacent_pos({X, Y}, n, {X, AY}) :- AY is Y - 1.
adjacent_pos({X, Y}, s, {X, AY}) :- AY is Y + 1.
adjacent_pos({X, Y}, e, {AX, Y}) :- AX is X + 1.
adjacent_pos({X, Y}, w, {AX, Y}) :- AX is X - 1.
adjacent_pos({X, Y}, ne, {AX, AY}) :- AX is X + 1, AY is Y - 1.
adjacent_pos({X, Y}, se, {AX, AY}) :- AX is X + 1, AY is Y + 1.
adjacent_pos({X, Y}, nw, {AX, AY}) :- AX is X - 1, AY is Y - 1.
adjacent_pos({X, Y}, sw, {AX, AY}) :- AX is X - 1, AY is Y + 1.
