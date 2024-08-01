:- initialization(main, main).

main :-
   part_1(Part1),
   format("Part 1: ~d~n", Part1),
   part_2(Part2),
   format("Part 2: ~d~n", Part2),
   run_tests.

:- begin_tests(day01).
test(part_1) :- part_1(1581).
test(part_2) :- part_2(1618).
:- end_tests(day01).

read_puzzle_input(Measurements) :-
   read_file_to_string("puzzle_inputs/day01.txt", Content, []),
   split_string(Content, "\n", "\n", Lines),
   maplist(number_string, Measurements, Lines).

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(Measurements),
   num_increasing_measurments(Measurements, "skip-0", Solution),
   !.

part_2(Solution) :-
   read_puzzle_input(Measurements),
   num_increasing_measurments(Measurements, "skip-2", Solution),
   !.

increasing_measurment(Measurements, Increasing, "skip-0") :-
   window([A, Increasing], Measurements),
   A < Increasing.

increasing_measurment(Measurements, Increasing, "skip-2") :-
   window([A, _, _, Increasing], Measurements),
   A < Increasing.

num_increasing_measurments(Measurements, CountMethod, N) :-
   bagof(Increasing, increasing_measurment(Measurements, Increasing, CountMethod), Bag),
   length(Bag, N).

window(W, L) :- append([_, W, _], L).
