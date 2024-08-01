:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

:- initialization(main, main).

main :-
   part_1(Part1),
   format("Part 1: ~d~n", Part1),
   part_2(Part2),
   format("Part 2: ~d~n", Part2),
   run_tests.

:- begin_tests(day02).
test(part_1) :- part_1(5710).
test(part_2) :- part_2(2259).
:- end_tests(day02).

read_puzzle_input(Motions) :-
   read_file_to_string("input.txt", Content, []),
   split_string(Content, "\n", "\n", Lines),
   maplist(motion, Lines, Motions).

motion(Line, Motion) :-
   string_to_list(Line, CharList),
   phrase(read_motion(Motion), CharList).

read_motion(Motion) -->
   string(DirectionStr), " ", integer(Magnitude), eos,
   { read_direction(Direction, DirectionStr),
     Motion = {Direction, Magnitude}
   }.

read_direction(right, R) :- string_to_list("R", R).
read_direction(left, L) :- string_to_list("L", L).
read_direction(up, U) :- string_to_list("U", U).
read_direction(down, D) :- string_to_list("D", D).

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(Motions),
   rope_head(Motions, HeadPositions),
   rope_tail(HeadPositions, TailPositions),
   visited_count(TailPositions, Solution),
   !.

part_2(Solution) :-
   read_puzzle_input(Motions),
   rope_head(Motions, HeadPositions),
   rope_tail_n(HeadPositions, TailPositions, 9),
   visited_count(TailPositions, Solution),
   !.

rope_head(Motions, Positions) :- rope_head([{0,0}], Motions, Positions).

rope_head(Visited, [], Positions) :- reverse(Visited, Positions).

rope_head(Visited, [{_Direction, 0} | Motions], Positions) :-
   rope_head(Visited, Motions, Positions).

rope_head(Visited, [Motion | Motions], Positions) :-
   [Prev | _] = Visited,
   move(Prev, Motion, Next, NextMotion),
   rope_head([Next | Visited], [NextMotion | Motions], Positions).

rope_tail([Start | HeadPositions], Positions) :- rope_tail([Start], HeadPositions, Positions).

rope_tail(Visited, [], Positions) :- reverse(Visited, Positions).

rope_tail(Visited, [Head | HeadPositions], Positions) :-
   [Prev | _] = Visited,
   follow(Prev, Head, Next),
   rope_tail([Next | Visited], HeadPositions, Positions).

rope_tail_n(LastTailPositions, LastTailPositions, 0).

rope_tail_n(HeadPositions, TailPositions, N) :-
   rope_tail(HeadPositions, NextPositions),
   rope_tail_n(NextPositions, TailPositions, NN),
   NN #= N - 1.

move({X1,Y}, {right, Mag}, {X2,Y}, {right, RestMag}) :-
   X2 #= X1 + 1, RestMag #= Mag - 1.

move({X1,Y}, {left, Mag}, {X2,Y}, {left, RestMag}) :-
   X2 #= X1 - 1, RestMag #= Mag - 1.

move({X,Y1}, {up, Mag}, {X,Y2}, {up, RestMag}) :-
   Y2 #= Y1 + 1, RestMag #= Mag - 1.

move({X,Y1}, {down, Mag}, {X,Y2}, {down, RestMag}) :-
   Y2 #= Y1 - 1, RestMag #= Mag - 1.

follow(Pos, Pos, Pos).

follow(Tail, Head, Tail) :- adjacent(Tail, Head).

follow({X1, Y1}, {X2, Y2}, {X3, Y3}) :-
   (X1 #< X2 -> X3 #= X1 + 1 ; X1 #> X2 -> X3 #= X1 - 1 ; X3 #= X1),
   (Y1 #< Y2 -> Y3 #= Y1 + 1 ; Y1 #> Y2 -> Y3 #= Y1 - 1 ; Y3 #= Y1).

adjacent({X1,Y1}, {X2,Y2}) :-
   X1 #>= X2 - 1, X1 #=< X2 + 1,
   Y1 #>= Y2 - 1, Y1 #=< Y2 + 1.

visited_count(PosList, Count) :-
   list_to_set(PosList, PosSet), length(PosSet, Count).
