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

:- begin_tests(day04).
test(part_1) :- part_1(67716).
test(part_2) :- part_2(1830).
:- end_tests(day04).

read_puzzle_input(BingoNumbers, Boards) :-
   read_file_to_string("puzzle_inputs/day04.txt", Content, []),
   string_to_list(Content, CharList),
   phrase(input_grammar(BingoNumbers, BoardLists), CharList),
   maplist(board, BoardLists, Boards).

input_grammar(BingoNumbers, Boards) -->
   sequence(integer, ",", BingoNumbers), eol, eol,
   sequence(board_grammar, "\n", Boards), eos.

board_grammar(Rows) --> sequence(row_grammar, Rows).

row_grammar([FirstSquare | Squares]) -->
   whites, integer(FirstSquare), whites,
   sequence(integer, (" ", whites), Squares), blank.

board(BoardList, BoardSet) :-
   setof(Square, square(BoardList, Square), BoardSet).

square(BoardList, {X, Y, Num, false}) :-
   nth0(Y, BoardList, Row),
   nth0(X, Row, Num).

%%%%%%%%%%%%%%%%%%%%%%%

part_1(Solution) :-
   read_puzzle_input(BingoNumbers, Boards),
   prefix(CalledNums, BingoNumbers),
   bingo(CalledNums, Boards, WinningBoard),
   score(CalledNums, WinningBoard, Solution),
   !.

part_2(Solution) :-
   read_puzzle_input(BingoNumbers, Boards),
   prefix(CalledNums, BingoNumbers),
   last_bingo(CalledNums, Boards, LastWinningBoard),
   score(CalledNums, LastWinningBoard, Solution),
   !.

bingo(CalledNumbers, Boards, WinningBoard) :-
   member(Board, Boards),
   append(PrevNums, [_LastNum], CalledNumbers),
   not(winning_board(PrevNums, Board, _)),
   winning_board(CalledNumbers, Board, WinningBoard).

last_bingo(CalledNumbers, Boards, LastWinningBoard) :-
   forall(member(Board, Boards), winning_board(CalledNumbers, Board, _)),
   bingo(CalledNumbers, Boards, LastWinningBoard).

score(CalledNumbers, Board, Score) :-
   append(_, [LastNum], CalledNumbers),
   findall(N, member({_,_,N,false}, Board), UnmarkedNumbers),
   sum_list(UnmarkedNumbers, Sum),
   Score #= LastNum * Sum.

mark_board(BingoNumbers, UnmarkedBoard, MarkedBoard) :-
   maplist(mark_square(BingoNumbers), UnmarkedBoard, MarkedBoard).

mark_square(BingoNumbers, {X, Y, Num, _}, {X, Y, Num, Marked}) :-
   member(Num, BingoNumbers) -> Marked = true ; Marked = false.

winning_board(BingoNumbers, Board, MarkedBoard) :-
   mark_board(BingoNumbers, Board, MarkedBoard),
   ( winning_row(MarkedBoard, _)
   ; winning_col(MarkedBoard, _)
   ).

winning_row(Board, Row) :-
   FixedPos in 0..4, indomain(FixedPos),
   MarkedSquare = {FixedPos, _, _, true},
   length(Row, 5),
   findall(MarkedSquare, member(MarkedSquare, Board), Row).

winning_col(Board, Col) :-
   FixedPos in 0..4, indomain(FixedPos),
   MarkedSquare = {_, FixedPos, _, true},
   length(Col, 5),
   findall(MarkedSquare, member(MarkedSquare, Board), Col).
