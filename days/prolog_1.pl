#!/usr/bin/env prolog

% 1st example
likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).

% 2nd example
food_type(velveeta, cheese).
food_type(ritz, cracker).
food_type(spam, meat).
food_type(sausage, meat).
food_type(jolt, soda).
food_type(twinkie, dessert).

flavor(sweet, dessert).
flavor(savory, meat).
flavor(savory, cheese).
flavor(sweet, soda).

food_flavor(X, Y) :- food_type(X, Z), flavor(Y, Z).

% 3rd example
different(red, green). different(red, blue).
different(green, red). different(green, blue).
different(blue, red). different(blue, green).

coloring(Alabama, Mississippi, Georgia, Tennessee, Florida) :-
    different(Mississippi, Tennessee),
    different(Mississippi, Alabama),
    different(Alabama, Tennessee),
    different(Alabama, Mississippi),
    different(Alabama, Georgia),
    different(Alabama, Florida),
    different(Georgia, Florida),
    different(Georgia, Tennessee).

% 4th example
cat(lion).
cat(tiger).
dorothy(X, Y, Z) :- X = lion, Y = tiger, Z = bear.
twin_cats(X, Y) :- cat(X), cat(Y).


% FIND
/* free Prolog tutorials:
http://www.cs.utexas.edu/~cannata/cs345/Class%20Notes/12%20prolog_intro.pdf (quick & dirty)
https://www.csupomona.edu/~jrfisher/www/prolog_tutorial/pt_framer.html (comprehensive)
http://www.dbnet.ece.ntua.gr/~adamo/csbooksonline/prolog-notes.pdf (very comprehensive, book)
http://kti.mff.cuni.cz/~bartak/prolog/contents.html (good website)

% support forum: http://gnu-prolog.996310.n3.nabble.com

% online reference to gnuprolog: http://www.gprolog.org
*/


% DO
% 1.
book('Head First Design Pattern').
book('Another Book').
author('Eric Freeman').
author('Elisabeth Freeman').
wrote('Eric Freeman', 'Head First Design Pattern').
wrote('Elisabeth Freeman', 'Head First Design Pattern').
wrote('Eric Freeman', 'Another Book').

% 2.
% QUERY: wrote('Eric Freeman', What).

% 3.
plays(jimi_hendrix, guitar).
plays(ringo_star, drums).
plays(my_friend, guitar).

genre(jimi_hendrix, rock).
genre(ringo_star, pop).
genre(my_friend, great_music).

% 4.
% QUERY: plays(Who, guitar).
