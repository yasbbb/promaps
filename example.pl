% This file contains a collections of test mazes
% Map predicates prefixed with "bad" should not work.
%
% display_map is a predicate that will pretty print
% a maze.
%
% When using the commandline you can provide swipl with multiple files.
%
% example use: ?- simple_map(M), display_map(M), find_exit(M).

simple_map([[s,f,e]]).
bad_map([[s],[e,w]]).
bad_map2([[s,f,w]]).
bad_map3([[s,e,e]]).
bad_map4([[s,s,e]]).
bad_map5([[w,f,e]]).


basic_map([[w,s,w],
           [f,f,w],
           [e,w,w]]).

basic_map2([[w,s,w],
            [f,f,w],
            [f,w,w],
            [f,f,e]]).

display_map(Map) :-
    Map = [Row|_], length(Row,L),
    write('    ▐'),display_line(L,'▁'),write('▍'),nl,
    display_rows(Map),
    write('    ▐'),display_line(L,'▔'),write('▍'),nl.

display_rows([]).
display_rows([Row|T]) :-
    write('    ▐'),
    display_row(Row),
    write('▍'),nl,
    display_rows(T).

display_row([]).
display_row([H|T]) :- display_symbol(H), display_row(T).

display_symbol(n) :- write('░'), !.
display_symbol(f) :- write(' '), !.
display_symbol(w) :- write('█'), !.
display_symbol(X) :- write(X).

display_line(0,_).
display_line(N,V) :- N>0, N2 is N-1, write(V), display_line(N2,V).
