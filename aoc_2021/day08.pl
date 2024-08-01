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

:- begin_tests(day08).
test(part_1) :- part_1(247).
test(part_2) :- part_2(933305).
:- end_tests(day08).

read_puzzle_input(Entries) :-
   read_file_to_string("puzzle_inputs/day08.txt", Content, []),
   string_to_list(Content, CharList),
   phrase(input_grammar(Entries), CharList),
   !.

input_grammar(Entries) --> sequence(entry_grammar, Entries), eos.

entry_grammar({SignalPatterns, OutputDigits}) -->
   sequence(signal_pattern_grammar, SignalPatterns), "|",
   sequence(output_digit_grammar, OutputDigits), "\n".

signal_pattern_grammar(SignalPattern) --> sequence(signal_grammar, SignalPattern), " ".

output_digit_grammar(OutputDigit) --> " ", sequence(signal_grammar, OutputDigit).

signal_grammar(Signal) --> nonblank(Char), { atom_codes(Signal, [Char]) }.

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(Entries),
   findall(Digit, (member(Entry, Entries), easy_digit(Entry, Digit)), EasyDigits),
   length(EasyDigits, Solution),
   !.

part_2(Solution) :-
   read_puzzle_input(Entries),
   findall(Value, (member(Entry, Entries), decode_entry(Entry, Value)), Values),
   sum_list(Values, Solution),
   !.

easy_digit({_, OutputDigits}, Digit) :-
   member(Digit, OutputDigits),
   once(digit(_, Digit, 1) ; digit(_, Digit, 4) ; digit(_, Digit, 7) ; digit(_, Digit, 8)).

decode_entry({SignalPatterns, OutputDigits}, Value) :-
   permutation([a,b,c,d,e,f,g], Mapping),
   forall(member(Pattern, SignalPatterns), digit(Mapping, Pattern, _)),
   maplist(digit(Mapping), OutputDigits, IntDigits),
   digits_number(IntDigits, Value).

digit([A, B, C, _, E, F, G], Signals, 0) :-
   length(Signals, 6), permutation([A, B, C, E, F, G], Signals).

digit([_, _, C, _, _, F, _], Signals, 1) :-
   length(Signals, 2), permutation([C, F], Signals).

digit([A, _, C, D, E, _, G], Signals, 2) :-
   length(Signals, 5), permutation([A, C, D, E, G], Signals).

digit([A, _, C, D, _, F, G], Signals, 3) :-
   length(Signals, 5), permutation([A, C, D, F, G], Signals).

digit([_, B, C, D, _, F, _], Signals, 4) :-
   length(Signals, 4), permutation([B, C, D, F], Signals).

digit([A, B, _, D, _, F, G], Signals, 5) :-
   length(Signals, 5), permutation([A, B, D, F, G], Signals).

digit([A, B, _, D, E, F, G], Signals, 6) :-
   length(Signals, 6), permutation([A, B, D, E, F, G], Signals).

digit([A, _, C, _, _, F, _], Signals, 7) :-
   length(Signals, 3), permutation([A, C, F], Signals).

digit([_, _, _, _, _, _, _], Signals, 8) :-
   length(Signals, 7).

digit([A, B, C, D, _, F, G], Signals, 9) :-
   length(Signals, 6), permutation([A, B, C, D, F, G], Signals).

digits_number(Digits, Number) :-
   reverse(Digits, LeastToGreatestDigits),
   findall(Digit * (10 ** I), nth0(I, LeastToGreatestDigits, Digit), Nums),
   sum_list(Nums, Number).
