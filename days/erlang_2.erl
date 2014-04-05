-module(erlang_2).
-export([get_value/2]).
-export([get_value_no_case/2]).
-export([get_value_list/2]).
-export([get_value_func/2]).
-export([total_price/1]).
-export([tic_tac_toe/1]).

% DO
% 1.
% it is generally interesting how many neat ways exist to express this funcitonality

%Tuples = [{erlang, "a functional language"}, {ruby, "an OO language"},
%		  {python, "my favorite language"}, {java, "ugly but prevailing language"}].

% in the end the best version I think
get_value(_, []) -> undefined;
get_value(Key, [Head|Tail]) -> case Head of
								   {Key, Value} -> Value;
								   {_, _} -> get_value(Key, Tail)
							   end.

% also great
get_value_no_case(_, []) -> undefined;
get_value_no_case(Key, [{Key, Value}|_]) -> Value;
get_value_no_case(Key, [_|Tail]) -> get_value_no_case(Key, Tail).

% nice but probably not what one would expect
get_value_list(Key, List) -> [ Value || {CurrentKey, Value} <- List, CurrentKey == Key ].

% nice but probably not as readable as the first two versions
get_final_value([]) -> undefined;
get_final_value([Head|_]) -> Head.
get_value_func(Key, List) -> get_final_value([ Value || {CurrentKey, Value} <- List, CurrentKey == Key ]).


% 2.
% easy...

% Shopping = [{book, 1, 49}, {beer, 10, 4}].
total_price(List) -> [ {Item, Quantity * Price} || {Item, Quantity, Price} <- List ].


% Bonus

% u stands for undefined
% Board1 = [o, x, u,
%			x, o, u,
%			x, u, o].
% Board2 = [o, x, o,
%			o, x, x,
%			x, o, o].
% Board3 = [o, x, u,
%			u, u, u,
%			o, x, u].
tic_tac_toe(Board) ->
	[One, Two, Three, Four, Five, Six, Seven, Eight, Nine] = Board,
	Cat = not lists:any(fun(X) -> X == u end, Board),
	if
		One == Two, One == Three, One /= u -> One;
		Four == Five, Four == Six, Four /= u -> Four;
		Seven == Eight, Seven == Nine, Seven /= u -> Seven;
		One == Four, One == Seven, One /= u -> One;
		Two == Five, Two == Eight, Two /= u -> Two;
		Three == Six, Three == Nine, Three /= u -> Three;
		One == Five, One  == Nine, One /= u -> One;
		Three == Five, Three == Seven, Three /= u -> Three;
		Cat -> cat;
		true -> no_winner
	end.
