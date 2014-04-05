-module(erlang_1).
-export([mirror/1]).
-export([number/1]).
-export([factorial/1]).
-export([fib/1]).
-export([number_of_words/1]).
-export([word_count/1]).
-export([count_to_ten/0]).
-export([error_or_success/1]).


mirror(Anything) -> Anything.

number(one) -> 1;
number(two) -> 2;
number(three) -> 3.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

% FIND
%
% official site: http://www.erlang.org
% function library: http://erlang.org/doc/man/STDLIB_app.html
% otp library: http://www.erlang.org/doc/
% OTP stands for "Open Telecom Platform" and is the open source distribution
%	of Erlang and an application server written in Erlang.

% DO
% 1.
number_of_words(S) -> number_of_words_rec(string:tokens(S, " ")).
number_of_words_rec([]) -> 0;
number_of_words_rec([_|Tail]) -> 1 + number_of_words_rec(Tail).
% alternatively make use of the knowledge that Strings are
% in fact Lists of numbers
word_count([]) -> 0;
word_count(S) -> word_count_rec(S, 1).
word_count_rec([], Count) -> Count;
word_count_rec([32|Tail], Count) -> word_count_rec(Tail, Count + 1);
word_count_rec([_|Tail], Count) -> word_count_rec(Tail, Count).

% 2.
count_to_ten() -> count_to_ten(1).
count_to_ten(10) -> io:write(10), io:fwrite("~n");
count_to_ten(N) -> io:write(N), io:fwrite(" "), count_to_ten(N + 1).

% 3.
error_or_success(success) -> io:fwrite("success~n");
% if Message is a number then it should be "~s: ~w"
error_or_success({error, Message}) -> io:fwrite("~p: ~p~n" , [error, Message]).

% The String representation as a List of numbers is awkward. Hopefully
% there is a good reason for this which I will encounter in the next chapters.
