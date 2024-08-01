:- use_module(library(dcg/basics)).

:- initialization(main, main).

main :-
  puzzle_input(Cypher),
  part_1(Cypher, Part1),
  format("Part 1: ~d~n", Part1).

:- begin_tests(day09).
test(part_1) :- puzzle_input(Cypher), part_1(Cypher, 20874512).
:- end_tests(day09).

puzzle_input(Numbers) :-
   read_file_to_string("puzzle_inputs/day09.txt", Content, []),
   split_string(Content, "\n", "\n", Lines),
   maplist(atom_number, Lines, Numbers).

part_1(Cypher, Solution) :-
  length(Cypher, Length),
  between(1, Length, Idx),
  aggregate(min(Idx), \+valid_xmas_cypher_num(Cypher, Idx), InvalidIdx),
  nth1(InvalidIdx, Cypher, Solution).

preamble_idx(Idx) :- between(1, 25, Idx).

addend_idx(Idx, AddendIdx) :-
  LowIdx is Idx - 25,
  HighIdx is Idx - 1,
  between(LowIdx, HighIdx, AddendIdx).

valid_xmas_cypher_num(_Cypher, Idx) :- preamble_idx(Idx).

valid_xmas_cypher_num(Cypher, Idx) :-
  addend_idx(Idx, FirstIdx),
  addend_idx(Idx, SecondIdx),
  FirstIdx \== SecondIdx,
  nth1(Idx, Cypher, Num),
  nth1(FirstIdx, Cypher, FirstAddend),
  nth1(SecondIdx, Cypher, SecondAddned),
  FirstAddend + SecondAddned =:= Num.
