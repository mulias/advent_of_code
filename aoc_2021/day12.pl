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

:- begin_tests(day12).
test(part_1) :- part_1(3708).
test(part_2) :- part_2(93858).
:- end_tests(day12).

read_puzzle_input(Connections) :-
   read_file_to_string("puzzle_inputs/day12.txt", Content, []),
   string_to_list(Content, CharList),
   phrase(input_grammar(Connections), CharList),
   !.

input_grammar(Connections) -->
   sequence(cave_connection_grammar, Connections), eos.

cave_connection_grammar(Cave1-Cave2) -->
   string_without("-", Cave1Name), "-", nonblanks(Cave2Name), eol,
   {atom_codes(Cave1, Cave1Name), atom_codes(Cave2, Cave2Name)}.

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(Connections),
   setof(Path, path(Connections, start, end, Path), Paths),
   length(Paths, Solution),
   !.

part_2(Solution) :-
   read_puzzle_input(Connections),
   setof(Path, path_with_extra_visit(Connections, start, end, Path), Paths),
   length(Paths, Solution),
   !.

small_cave(CaveName) :- string_lower(CaveName, CaveName).

path(_Connections, EndCave, EndCave, [EndCave]).

path(Connections, StartCave, EndCave, Path) :-
   select_connection(StartCave, NextCave, Connections, RestConnections),
   (small_cave(StartCave) ->
      path(RestConnections, NextCave, EndCave, NextPath)
   ;  path(Connections, NextCave, EndCave, NextPath)
   ),
   Path = [StartCave | NextPath].

path_with_extra_visit(Connections, StartCave, EndCave, Path) :-
   setof(Cave, extra_visit_cave(Connections, Cave), ExtraVisitCaves),
   member(ExtraVisitCave, ExtraVisitCaves),
   path_with_extra_visit(Connections, StartCave, EndCave, ExtraVisitCave, Path).

path_with_extra_visit(_Connections, EndCave, EndCave, _ExtraVisitCave, [EndCave]).

path_with_extra_visit(Connections, ExtraVisitCave, EndCave, ExtraVisitCave, Path) :-
   select_connection(ExtraVisitCave, NextCave, Connections, _RestConnections),
   path(Connections, NextCave, EndCave, NextPath),
   Path = [ExtraVisitCave | NextPath].

path_with_extra_visit(Connections, StartCave, EndCave, ExtraVisitCave, Path) :-
   select_connection(StartCave, NextCave, Connections, RestConnections),
   (small_cave(StartCave) ->
      path_with_extra_visit(RestConnections, NextCave, EndCave, ExtraVisitCave, NextPath)
   ;  path_with_extra_visit(Connections, NextCave, EndCave, ExtraVisitCave, NextPath)
   ),
   Path = [StartCave | NextPath].

select_connection(Cave1, Cave2, Connections, RestConnections) :-
   (member(Cave1-Cave2, Connections) ; member(Cave2-Cave1, Connections)),
   remove_cave(Connections, Cave1, RestConnections).

remove_cave(Connections, Cave, ConnectionsWithoutCave) :-
   delete(Connections, Cave-_, C), delete(C, _-Cave, ConnectionsWithoutCave).

extra_visit_cave(Connections, Cave) :-
   (member(Cave-_, Connections) ; member(_-Cave, Connections)),
   small_cave(Cave), Cave \= start, Cave \= end.
