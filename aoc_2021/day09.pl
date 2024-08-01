:- use_module(library(clpfd)).
:- use_module(library(dcg/basics), except([digit/3])).
:- use_module(library(dcg/high_order)).

:- set_prolog_flag(answer_write_options,[max_depth(0)]).

:- initialization(main, main).

main :-
   part_1(Part1),
   format("Part 1: ~d~n", Part1),
   part_2(Part2),
   format("Part 2: ~d~n", Part2),
   run_tests.

:- begin_tests(day09).
test(part_1) :- part_1(_).
test(part_2) :- part_2(_).
:- end_tests(day09).

%%%%%%%%%%%%%%%%%%%%%%%
