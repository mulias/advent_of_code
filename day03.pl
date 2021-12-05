:- use_module(library(clpfd)).

:- initialization(main, main).

main :-
   part_1(Part1),
   format("Part 1: ~d~n", Part1),
   part_2(Part2),
   format("Part 2: ~d~n", Part2),
   run_tests.

:- begin_tests(day01).
test(part_1) :- part_1(2498354).
test(part_2) :- part_2(3277956).
:- end_tests(day01).

% Make a list of lists of bits, e.g. `[[1,0,1|...],[0,1,1|...]|...]`.
read_puzzle_input(Binaries) :-
   read_file_to_string("puzzle_inputs/day03.txt", Content, []),
   split_string(Content, "\n", "\n", Lines),
   maplist(string_bits, Lines, Binaries).

string_bits(String, Bits) :-
   string_codes(String, Codes),
   maplist(bit_code, Bits, Codes).

bit_code(Bit, Code) :-
   number_codes(Bit, [Code]).

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(Binaries),
   binaries_tree(Binaries, Tree),
   gamma_rate(Tree, GammaBinary),
   power_consumption(GammaBinary, Solution),
   !.

part_2(Solution) :-
   read_puzzle_input(Binaries),
   binaries_tree(Binaries, Tree),
   oxygen_rating(Tree, OxygenBinary),
   co2_rating(Tree, CO2Binary),
   life_support_rating(OxygenBinary, CO2Binary, Solution),
   !.

% Most common bit at each depth of the tree/position in bit lists.
gamma_rate(Tree, Gamma) :-
   is_binary(Gamma), label(Gamma),
   forall(
      nth1(N, Gamma, Bit),
      global_common_bit(Tree, N, Bit)
   ).

% Decimal product of gamma and epsilon.
power_consumption(GammaBinary, Consumption) :-
   maplist(flip, GammaBinary, EpsilonBinary),
   bits_number(GammaBinary, G),
   bits_number(EpsilonBinary, E),
   Consumption #= G * E.

% Bit list in tree such that for each node the next bit is the most common bit
% of the node.
oxygen_rating(Tree, Oxygen) :-
   is_binary(Oxygen), label(Oxygen),
   forall(
      append([Prefix, [Bit], _], Oxygen),
      local_common_bit(Prefix, Tree, Bit)
   ).

% Bit list in tree such that for each node the next bit is the least common bit
% of the node.
co2_rating(Tree, CO2) :-
   is_binary(CO2), label(CO2),
   forall(
      append([Prefix, [Bit], _], CO2),
      local_least_common_bit(Prefix, Tree, Bit)
   ).

% Decimal product of oxygen and co2.
life_support_rating(OxygenBinary, CO2Binary, Rating) :-
   bits_number(OxygenBinary, O),
   bits_number(CO2Binary, C),
   Rating #= O * C.

% A list is bits if every element is 0 or 1.
is_bits(Bits) :- Bits ins 0..1.

% In the world of this puzzle all binaries are lists of 12 bits.
is_binary(Binary) :- length(Binary, 12), is_bits(Binary).

% A tree is a leaf or a node with two subtrees. In this case the subtrees are
% used to encode bit lists with a leading zero or leading one. We use the node
% label to count total nodes inserted into the tree.
tree(leaf).
tree(node(_Count, _Zero, _One)).
node(_Count, Zero, One) :- tree(Zero), tree(One).

% Tree encoding all of the bit lists in Binaries.
binaries_tree(Binaries, Tree) :-
   foldl(insert, Binaries, leaf, Tree).

% Tree encoding a bit list
insert([0 | Bits], leaf, node(1, ZeroTree, leaf)) :-
   insert(Bits, leaf, ZeroTree).

insert([0 | Bits], node(Count, Zero, One), node(Count + 1, ZeroTree, One)) :-
   insert(Bits, Zero, ZeroTree).

insert([1 | Bits], leaf, node(1, leaf, OneTree)) :-
   insert(Bits, leaf, OneTree).

insert([1 | Bits], node(Count, Zero, One), node(Count + 1, Zero, OneTree)) :-
   insert(Bits, One, OneTree).

insert([], leaf, node(1, leaf, leaf)).

insert([], node(Count, Zero, One), node(Count + 1, Zero, One)).

% Subtree prefixed in the parent tree by a bit list.
subtree([0 | Bits], node(_, Zero, _), Subtree) :-
   subtree(Bits, Zero, Subtree).

subtree([1 | Bits], node(_, _, One), Subtree) :-
   subtree(Bits, One, Subtree).

subtree([_ | _], leaf, leaf).

subtree([], Tree, Tree).

% Number of nodes in the subtree prefixed in the parent tree by a bit list.
count(Bits, Tree, Count) :-
   subtree(Bits, Tree, Subtree),
   (node(C, _, _) = Subtree -> Count #= C
   ; Count #= 0
   ).

% Sum of all Count values where `count(Bits, Tree, Count)` is true.
count_all(Bits, Tree, Total) :-
   findall(Count, count(Bits, Tree, Count), Counts),
   sum_list(Counts, Total).

% The most common bit at position/depth N for all bit lists in Tree.
global_common_bit(Tree, N, Bit) :-
   count_all([], Tree, InputsCount),
   length(PrefixBits, N), nth1(N, PrefixBits, 0),
   count_all(PrefixBits, Tree, SubtreeCount),
   (SubtreeCount #> InputsCount // 2 -> Bit #= 0 ; Bit #= 1).

% The most common bit inserted into the node prefixed in the parent Tree by
% Bits.
local_common_bit(Bits, Tree, CommonBit) :-
   subtree(Bits, Tree, Subtree),
   count([0], Subtree, ZeroCount),
   count([1], Subtree, OneCount),
   ( ZeroCount #= 0, OneCount #= 0 -> false
   ; ZeroCount #> OneCount -> CommonBit #= 0
   ; CommonBit #= 1
   ).

% The least common bit inserted into the node prefixed in the parent Tree by
% Bits. Takes into account that "least common" still means extant.
local_least_common_bit(Bits, Tree, LeastCommonBit) :-
   subtree(Bits, Tree, Subtree),
   count([0], Subtree, ZeroCount),
   count([1], Subtree, OneCount),
   ( ZeroCount #= 0, OneCount #= 0 -> false
   ; ZeroCount #= 0 -> LeastCommonBit #= 1
   ; OneCount #= 0 -> LeastCommonBit #= 0
   ; ZeroCount #=< OneCount -> LeastCommonBit #= 0
   ; LeastCommonBit #= 1
   ).

% Conversion from list of bits to decimal number.
bits_number(Bits, Number) :-
   reverse(Bits, LeastToGreatestBits),
   findall(Bit * (2 ** I), nth0(I, LeastToGreatestBits, Bit), Nums),
   sum_list(Nums, Number).

% Flipflop.
flip(0, 1).
flip(1, 0).
