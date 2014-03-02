#!/usr/bin/env prolog

/* good use case:
* solving systems with constraints
* or in other words: we have a set of constraints which are easy to express but
*   hard to solve
*/

/*
% Sudoku
valid([]).
valid([Head|Tail]) :-
    fd_all_different(Head),
    valid(Tail).

sudoku(Puzzle, Solution) :-
    Solution = Puzzle,

    Puzzle = [S11, S12, S13, S14,
              S21, S22, S23, S24,
              S31, S32, S33, S34,
              S41, S42, S43, S44],

    Row1 = [S11, S12, S13, S14],
    Row2 = [S21, S22, S23, S24],
    Row3 = [S31, S32, S33, S34],
    Row4 = [S41, S42, S43, S44],

    Col1 = [S11, S21, S31, S41],
    Col2 = [S12, S22, S32, S42],
    Col3 = [S13, S23, S33, S43],
    Col4 = [S14, S24, S34, S44],

    Square1 = [S11, S12, S21, S22],
    Square2 = [S13, S14, S23, S24],
    Square3 = [S31, S32, S41, S42],
    Square4 = [S33, S34, S43, S44],

    fd_domain(Puzzle, 1, 4),

    valid([Row1, Row2, Row3, Row4,
           Col1, Col2, Col3, Col4,
           Square1, Square2, Square3, Square4]).
*/

% Eight Queens Problem
/*
% my try without looking at solution
eigth_queens(Positions) :-
    Positions = [P1, P2, P3, P4, P5, P6, P7, P8],
    P1 = (Row1, Col1),
    P2 = (Row2, Col2),
    P3 = (Row3, Col3),
    P4 = (Row4, Col4),
    P5 = (Row5, Col5),
    P6 = (Row6, Col6),
    P7 = (Row7, Col7),
    P8 = (Row8, Col8),
    fd_domain([Row1, Col1, Row2, Col2, Row3, Col3, Row4, Col4,
               Row5, Col5, Row6, Col6, Row7, Col7, Row8, Col8], 1, 8),
    fd_all_different([Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8]),
    fd_all_different([Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8]),

    % diagonals are tricky...lets look at the solution
    % I wasn't able to find a solution to diagonals as my thinking was wrong!!
    %   I should have thought about what exactly defines a diagonal,
    %   e.g. Diagonal is Col - Row,
    %   and not about the specific operations needed to get a diagonal,
    %   e.g. Col + 1, Row + 1.
*/
/* basic
valid_queen((Row, Col)) :-
    Range = [1,2,3,4,5,6,7,8],
    member(Row, Range), member(Col, Range).
*/
% optimized
valid_queen((_, Col)) :- member(Col, [1,2,3,4,5,6,7,8]).

valid_board([]).
valid_board([Head|Tail]) :- valid_queen(Head), valid_board(Tail).

/* basic
rows([], []).
rows([(Row, _)|QueensTail], [Row|RowsTail]) :-
    rows(QueensTail, RowsTail).
*/

cols([], []).
cols([(_, Col)|QueensTail], [Col|ColsTail]) :-
    cols(QueensTail, ColsTail).

diags1([], []).
diags1([(Row, Col)|QueensTail], [Diagonal|DiagonalsTail]) :-
    Diagonal is Col - Row,
    diags1(QueensTail, DiagonalsTail).

diags2([], []).
diags2([(Row, Col)|QueensTail], [Diagonal|DiagonalsTail]) :-
    Diagonal is Col + Row,
    diags2(QueensTail, DiagonalsTail).

/* basic
eight_queens(Queens) :-
    length(Queens, 8),
    valid_board(Queens),

    rows(Queens, Rows),
    cols(Queens, Cols),
    diags1(Queens, Diags1),
    diags2(Queens, Diags2),

    fd_all_different(Rows),
    fd_all_different(Cols),
    fd_all_different(Diags1),
    fd_all_different(Diags2).
*/
% optimized
eight_queens(Queens) :-
    Queens = [(1, _), (2, _), (3, _), (4, _), (5, _), (6, _), (7, _), (8, _)],
    valid_board(Queens),

    cols(Queens, Cols),
    diags1(Queens, Diags1),
    diags2(Queens, Diags2),

    fd_all_different(Cols),
    fd_all_different(Diags1),
    fd_all_different(Diags2).



% FIND
% print predicates: write, print, format
rev([], []).
rev([Head|Tail], Rev) :- 
    rev(Tail, TailRev),
    append(TailRev, [Head], Rev),
    write(Rev), nl.
% print only successful solutions: doesn't make sense?!


% DO
% sudoku 6x6 - 9x9 is the same
% Sudoku
valid([]).
valid([Head|Tail]) :-
    fd_all_different(Head),
    valid(Tail).

pretty_print([Head|Tail]) :-
    write(Head), nl,
    pretty_print(Tail).

sudoku(Puzzle, Solution) :-
    Solution = Puzzle,

    Puzzle = [S11, S12, S13, S14, S15, S16,
              S21, S22, S23, S24, S25, S26,
              S31, S32, S33, S34, S35, S36,
              S41, S42, S43, S44, S45, S46,
              S51, S52, S53, S54, S55, S56,
              S61, S62, S63, S64, S65, S66],

    Row1 = [S11, S12, S13, S14, S15, S16],
    Row2 = [S21, S22, S23, S24, S25, S26],
    Row3 = [S31, S32, S33, S34, S35, S36],
    Row4 = [S41, S42, S43, S44, S45, S46],
    Row5 = [S51, S52, S53, S54, S55, S56],
    Row6 = [S61, S62, S63, S64, S65, S66],

    Col1 = [S11, S21, S31, S41, S51, S61],
    Col2 = [S12, S22, S32, S42, S52, S62],
    Col3 = [S13, S23, S33, S43, S53, S63],
    Col4 = [S14, S24, S34, S44, S54, S64],
    Col5 = [S15, S25, S35, S45, S55, S65],
    Col6 = [S16, S26, S36, S46, S56, S66],

    Square1 = [S11, S12, S13, S21, S22, S23],
    Square2 = [S14, S15, S16, S24, S25, S26],
    Square3 = [S31, S32, S33, S41, S42, S43],
    Square4 = [S34, S35, S36, S44, S45, S46],
    Square5 = [S51, S52, S53, S61, S62, S63],
    Square6 = [S54, S55, S56, S64, S65, S66],

    fd_domain(Puzzle, 1, 6),

    valid([Row1, Row2, Row3, Row4, Row5, Row6,
           Col1, Col2, Col3, Col4, Col5, Col6,
           Square1, Square2, Square3, Square4, Square5, Square6]),

    pretty_print([Row1, Row2, Row3, Row4, Row5, Row6]).

/* TODO
* research meaning of "not"
* is predicate vs unification --> fully grounded?!
* build better sudoku (find bug in current sudoku?!)
* check out advanced eight queens problem
*/
