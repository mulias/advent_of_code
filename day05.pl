:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

main :-
   part_1(Part1),
   format("Part 1: ~d~n", Part1),
   part_2(Part2),
   format("Part 2: ~d~n", Part2),
   run_tests.

:- begin_tests(day01).
test(part_1) :- part_1(6007).
test(part_2) :- part_2(19349).
:- end_tests(day01).

read_puzzle_input(Lines) :-
   read_file_to_string("puzzle_inputs/day05.txt", Content, []),
   string_to_list(Content, CharList),
   phrase(input_grammar(Lines), CharList),
   !.

input_grammar(Lines) -->
   sequence(line_grammar, Lines), eos.

line_grammar({X1,Y1}-{X2,Y2}) -->
   integer(X1), ",", integer(Y1), " -> ", integer(X2), ",", integer(Y2), eol.

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(AllLines),
   without_diagonals(AllLines, Lines),
   num_intersection_points(Lines, Solution),
   !.

part_2(Solution) :-
   read_puzzle_input(Lines),
   num_intersection_points(Lines, Solution),
   !.

without_diagonals(Lines, NonDiagonals) :-
   findall(Line, (member(Line, Lines), non_diagonal_line(Line)), NonDiagonals).

non_diagonal_line({X,_}-{X,_}).
non_diagonal_line({_,Y}-{_,Y}).

num_intersection_points(Lines, Count) :-
   setof(Point, intersection(Lines, Point), Intersections),
   length(Intersections, Count).

intersection(Lines, Point) :-
   unordered_pair(Lines, {Line1, Line2}),
   intersection(Line1, Line2, Point).

intersection(Line1, Line2, {X,Y}) :-
   line_point(Line1, {X1,Y1}),
   line_point(Line2, {X2,Y2}),
   X #= X1, X #= X2, indomain(X),
   Y #= Y1, Y #= Y2, indomain(Y).

line_point({X,Y}-{X,Y}, {X,Y}).

line_point({X1,Y}-{X2,Y}, {X,Y}) :-
   X1 #\= X2,
   low_high({X1,X2}, {XL,XH}), X in XL..XH.

line_point({X,Y1}-{X,Y2}, {X,Y}) :-
   Y1 #\= Y2,
   low_high({Y1,Y2}, {YL,YH}),
   Y in YL..YH.

line_point({X1,Y1}-{X2,Y2}, Point) :-
   X1 #\= X2, Y1 #\= Y2,
   (X1 #> X2 ->
      line_point({X2,Y2}-{X1,Y1}, Point)
   ;  Diff #= X2 - X1, N in 0..Diff,
      (Y1 #< Y2 ->
         Point = {X1 + N, Y1 + N}
      ;  Point = {X1 + N, Y1 - N}
      )
   ).

low_high({A,B}, {L,H}) :-
   (A #=< B ->
      L = A, H = B
   ;  L = B, H = A
   ).

unordered_pair([A | List], Pair) :-
   member(B, List), Pair = {A,B} ; unordered_pair(List, Pair).
