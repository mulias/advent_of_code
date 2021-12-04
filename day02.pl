:- use_module(library(dcg/basics)).

:- initialization(main, main).

main :-
   part_1(Part1),
   format("Part 1: ~d~n", Part1),
   part_2(Part2),
   format("Part 2: ~d~n", Part2),
   run_tests.

:- begin_tests(day08).
test(part_1) :- part_1(1580000).
test(part_2) :- part_2(1251263225).
:- end_tests(day08).

read_puzzle_input(Commands) :-
   read_file_to_string("puzzle_inputs/day02.txt", Content, []),
   split_string(Content, "\n", "\n", Lines),
   maplist(command, Lines, Commands).

command(Line, Command) :-
   string_to_list(Line, CharList),
   phrase(read_command(Command), CharList).

read_command(Command) -->
   string(Operation), " ", integer(Arg), eos,
   { atom_codes(OperationAtom, Operation),
     Command = {OperationAtom, Arg}
   }.

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(Commands),
   course(Commands, {0, 0}, {Horizontal, Depth}),
   Solution is Horizontal * Depth,
   !.

part_2(Solution) :-
   read_puzzle_input(Commands),
   course(Commands, {0, 0, 0}, {Horizontal, Depth, _Aim}),
   Solution is Horizontal * Depth,
   !.

course([], EndPos, EndPos).

course([Command | Commands], StartPos, EndPos) :-
   move(Command, StartPos, NextPos),
   course(Commands, NextPos, EndPos).

move({forward, Delta}, {H, D, A}, {H + Delta, D + (Delta * A), A}).

move({up, Delta}, {H, D, A}, {H, D, A - Delta}).

move({down, Delta}, {H, D, A}, {H, D, A + Delta}).

move({forward, Delta}, {H, D}, {H + Delta, D}).

move({up, Delta}, {H, D}, {H, D - Delta}).

move({down, Delta}, {H, D}, {H, D + Delta}).
