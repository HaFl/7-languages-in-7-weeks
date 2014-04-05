#!/usr/bin/env prolog

% experience: debugging is a nightmare as Prolog doesn't give you any hint and
%   usually just answers with "no" (which is funny at first, but ...)

% 1st example - recursion
father(zeb, john_boy_sr).
father(john_boy_sr, john_boy_jr).

ancestor(X, Y) :- father(X, Y).
ancestor(X, Y) :- father(X, Z), ancestor(Z, Y).

% lists and math
count(0, []).
count(Count, [_|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

sum(0, []).
sum(Total, [Head|Tail]) :- sum(Sum, Tail), Total is Head + Sum.

average(Average, List) :- sum(Sum, List), count(Count, List), Average is Sum/Count.

/* usage of append:
append([a], [b], [a, c]).     % lie detector
append([a], [b], What).       % list builder
append([a], Who, [a, b]).     % list subtraction
append(One, Two, [a, b, c]).  % permutations
*/
concatenate([], List, List).
/* general process
concatenate([Head|[]], List, [Head|List]).
concatenate([Head1|[Head2|[]]], List, [Head1, Head2|List]).
concatenate([Head1|[Head2|[Head3|[]]]], List, [Head1, Head2, Head3|List]).
*/
concatenate([Head|Tail1], List, [Head|Tail2]) :-
    concatenate(Tail1, List, Tail2).


% FIND
% Fibonacci
/*
* % fibonacci - naive
* fib(0, 0).
* fib(1, 1).
* fib(N, NF) :- A is N - 1, B is N - 2,
*     fib(A, AF), fib(B, BF),
*     NF is AF + BF.
*/
% fibonacci - tail recursive
fib(N, NF) :- fib(N, 0, 1, NF).
fib(0, A, _, A).
fib(N, A, B, NF) :- N1 is N - 1, Sum is A + B, fib(N1, B, Sum, NF).
% damn, I really like this way of thinking --> very similar to functional style

/*
* % factorial - naive - order of facts in rule is important!
* fac(0, 1).
* fac(N, F) :- N1 is N - 1, fac(N1, F1), F is N * F1.
*/
fac(N, R) :- fac(N, 1, R).
fac(0, R, R).
fac(N, F, R) :- N1 is N - 1, F1 is N * F, fac(N1, F1, R).

/* real world community: https://github.com/search?o=desc&q=language%3Aprolog&ref=cmdform&s=&type=Repositories
* - music ontology (semantic web)
* - owl2 library (web ontology language)
* - parser for English
*/

% Towers of Hanoi - maybe some other time

% Negation:
%   my first guess --> "not X" means "everything except X". So you might select more
%       options than intended.
%   actually --> not(X) is the negation of X, but does not mean that X is false,
%       it means that X can't be proven true
%       example: man(Adam). woman(Eve). Query: not(man(Abel)) --> yes
%       Also, negating a predicate can cause subsequent predicates to be
%       ignored (due to 'cut' and 'fail').

% DO
/* first shot
% reverse a list
rev([], []).
rev([Head|List1], [List2|Head]) :- rev(List1, List2).
*/
% reverse a list - better
rev([], []).
rev([Head|Tail], Rev) :- rev(Tail, TailRev), append(TailRev, [Head], Rev).

% smallest element of list - no if else in Prolog?!
min(Head, [Head|[]]).
% could be done "nicer" by replacing last fact with adequate arguments, but in
% this case I think it is more readable
min(M, [Head|Tail]) :- min(M1, Tail), M1 =< Head, M is M1.
min(M, [Head|Tail]) :- min(M1, Tail), M1 > Head, M is Head.

% sort list
removeFromList(E, [E|R], R).
removeFromList(E, [F|R], [F|S]) :- removeFromList(E, R, S).
mySort([], []).
mySort(List, [Min|Sorted]) :-
    min(Min, List),
    removeFromList(Min, List, Rest),
    mySort(Rest, Sorted).
