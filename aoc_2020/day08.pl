:- use_module(library(dcg/basics)).

:- initialization(main, main).

main :-
  setup,
  part_1(Part1),
  format("Part 1: ~d~n", Part1),
  part_2(Part2),
  format("Part 2: ~d~n", Part2),
  run_tests.

:- begin_tests(day08).
test(part_1) :- part_1(1675).
test(part_2) :- part_2(1532).
:- end_tests(day08).

% Assert facts that look like this:
%   instruction(0, nop, 0).
%   instruction(1, acc, 1).
%   instruction(2, jmp, 4).
setup :-
  read_puzzle_input("puzzle_inputs/day08.txt", Lines),
  map_with_index(instruction_fact, Lines, Facts),
  maplist(assert, Facts).

read_puzzle_input(Filename, Lines) :-
   read_file_to_string(Filename, Content, []),
   split_string(Content, "\n", "\n", Lines).

instruction_fact(Line, LineNum, Fact) :-
  string_to_list(Line, CharList),
  phrase(read_instruction_fact(LineNum, Fact), CharList).

read_instruction_fact(LineNum, Fact) -->
  string(Operation), " ", integer(Arg), eos,
  { atom_codes(OperationAtom, Operation),
    Fact =.. [instruction, LineNum, OperationAtom, Arg]
  }.

map_with_index(_Fn, _I, [], []).
map_with_index(Fn, I, [H|T], [HO|TO]) :-
  NextI is I + 1,
  call(Fn, H, I, HO),
  map_with_index(Fn, NextI, T, TO).
map_with_index(Fn, List, ListOut) :- map_with_index(Fn, 0, List, ListOut).

%%%%%%%%%%%%%%%%%%%%%%%

% Start at step 0, accumulator 0, and no completed steps.
start_state({0, 0, []}).

next_state(State, NextState) :-
  {Step, _Accumulator, _StepsSoFar} = State,
  instruction(Step, Op, Arg),
  perform(Op, Arg, State, NextState).

perform(acc, Arg, {Step, Accumulator, StepsSoFar}, {NextStep, NextAccumulator, Steps}) :-
  NextStep is Step + 1,
  NextAccumulator is Accumulator + Arg,
  Steps = [Step | StepsSoFar].
perform(nop, _Arg, {Step, Accumulator, StepsSoFar}, {NextStep, NextAccumulator, Steps}) :-
  NextStep is Step + 1,
  NextAccumulator is Accumulator,
  Steps = [Step | StepsSoFar].
perform(jmp, Arg, {Step, Accumulator, StepsSoFar}, {NextStep, NextAccumulator, Steps}) :-
  NextStep is Step + Arg,
  NextAccumulator is Accumulator,
  Steps = [Step | StepsSoFar].

step_revisted(State, NextState) :-
  {_Step, _Accumulator, StepsSoFar} = State,
  {NextStep, _NextAccumulator, _Steps} = NextState,
  member(NextStep, StepsSoFar).

non_looping_path(StartState, EndState) :-
  next_state(StartState, EndState).
non_looping_path(StartState, EndState) :-
  next_state(StartState, NextState),
  \+step_revisted(StartState, NextState),
  non_looping_path(NextState, EndState).

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
  start_state(StartState),
  infinite_loop(StartState, {_LoopStep, LoopAccumulator, _Steps}),
  Solution is LoopAccumulator,
  !.

infinite_loop(StartState, LoopState) :-
  non_looping_path(StartState, PreLoopState),
  next_state(PreLoopState, LoopState),
  step_revisted(PreLoopState, LoopState).

%%%%%%%%%%%%%%%%%%%%%%%

part_2(Solution) :-
  start_state(StartState),
  end_step(EndStep),
  path_with_one_swap(StartState, _SwapState, {EndStep, EndAccumulator, _Steps}),
  Solution is EndAccumulator,
  !.

swapped_op(nop, jmp).
swapped_op(jmp, nop).

next_state_swapped_op(State, NextState) :-
  {Step, _Accumulator, _StepsSoFar} = State,
  instruction(Step, Op, Arg),
  swapped_op(Op, SwappedOp),
  perform(SwappedOp, Arg, State, NextState).

path_with_one_swap(StartState, SwapState, EndState) :-
  non_looping_path(StartState, SwapState),
  next_state_swapped_op(SwapState, NextState),
  non_looping_path(NextState, EndState).

end_step(EndStep) :-
  aggregate_all(max(X), instruction(X, _, _), LastStep),
  EndStep is LastStep + 1.
