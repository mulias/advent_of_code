:- use_module(library(dcg/basics)).

:- initialization(main, main).

main :-
  setup,
  part_1(Part1),
  format("Part 1: ~d~n", Part1),
  part_2(Part2),
  format("Part 2: ~d~n", Part2),
  run_tests.

:- begin_tests(day07).
test(part_1) :- part_1(296).
test(part_2) :- part_2(9339).
:- end_tests(day07).

% Assert facts that look like this:
%   contain(drabpurple, lightgreen, 2).
%   contain(dimolive, darkbeige, 1).
%   contain(wavyred, mirroredteal, 3).
setup :-
  read_puzzle_input("puzzle_inputs/day07.txt", Lines),
  maplist(assert_bag_facts, Lines).

read_puzzle_input(Filename, Lines) :-
   read_file_to_string(Filename, Content, []),
   split_string(Content, "\n", "\n", Lines).

assert_bag_facts(Line) :-
  string_to_list(Line, CharList),
  findall(Fact, phrase(read_bag_fact(Fact), CharList), Facts),
  maplist(assert, Facts).

read_bag_fact(Fact) -->
  nonblanks(OuterBagAdjective), " ", nonblanks(OuterBagColor), " bags contain ",
  string(_),
  integer(InnerBagNum), " ", nonblanks(InnerBagAdjective), " ", nonblanks(InnerBagColor), " bag",
  string(_),
  { append(OuterBagAdjective, OuterBagColor, OuterBag),
    atom_codes(OuterBagAtom, OuterBag),
    append(InnerBagAdjective, InnerBagColor, InnerBag),
    atom_codes(InnerBagAtom, InnerBag),
    Fact =.. [contain, OuterBagAtom, InnerBagAtom, InnerBagNum]
  }.

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :- num_bigger_bags(shinygold, Solution), !.

bigger_bag(Outer,Inner) :- contain(Outer,Inner,_).
bigger_bag(Outer,Inner) :- contain(Outer,Middle,_), bigger_bag(Middle,Inner).

num_bigger_bags(Inner,N) :- setof(Outer, bigger_bag(Outer, Inner), Set), length(Set, N).

%%%%%%%%%%%%%%%%%%%%%%%

part_2(Solution) :- bag_size(shinygold, Solution), !.

bag_size(Bag,N) :- \+contain(Bag, _, _), N is 0.
bag_size(Bag,N) :-
  findall({Inner, NumInner}, contain(Bag, Inner, NumInner), InnerBags),
  maplist(inner_bag_size, InnerBags, InnerCounts),
  sum_list(InnerCounts, N).

inner_bag_size({InnerBag, NumInner}, N) :-
  bag_size(InnerBag, SizeInner),
  N is NumInner * (SizeInner + 1).
