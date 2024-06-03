% This file provides the gen_map/4 predicate.
% It takes 4 arguments:
%   1. The number of iterations the algorithm should go through, 4 is a good value
%   2. The number of rows the map (maze) should have
%   3. The number of columns the map (maze) should have
%   4. Is the random map (maze)
%
% When using the commandline you can provide swipl with multiple files.
%
% Example Usage: ?- gen_map(4, 10,10,M), find_exit(M,A).

:- module(test,[gen_map/4,show_random_map/3]).

make_base(0,_,[]).
make_base(N,Cols,[Row|T]) :-
    N > 0, N2 is N-1,
    make_rows(Cols, Row),
    make_base(N2, Cols, T).

make_rows(0,[]).
make_rows(N,[n|T]) :-
    N > 0, N2 is N-1,
    make_rows(N2,T).

nth([H|_], 0, H).
nth([_|T], N, Value) :- 
    N > 0, N2 is N-1,
    nth(T,N2,Value).

set_nth([_|T], 0, V, [V|T]).
set_nth([H|T], N, V, [H|T2]) :-
    N > 0, N2 is N-1,
    set_nth(T,N2,V,T2).

cell(Map, coord(R, C), Value) :-
    nth(Map,R,Row),
    nth(Row,C,Value).

set_cell(Map, coord(R,C),Value,NewMap) :-
    nth(Map,R,Row),
    set_nth(Row,C,Value,NewRow),
    set_nth(Map,R,NewRow,NewMap).

set_cell_safe(Map, Coord, Value, NewMap) :-
	cell(Map,Coord,n),!,
	set_cell(Map,Coord,Value,NewMap).
set_cell_safe(Map, _, _, Map).

horizontal_wall(Map, R, C, C, NewMap) :-
    set_cell_safe(Map, coord(R,C), w, NewMap).
horizontal_wall(Map, R, C1,C2, NewMap) :-
    C1 < C2 , C3 is C1+1,
    set_cell_safe(Map, coord(R,C1), w, Map2),
    horizontal_wall(Map2, R, C3,C2, NewMap).

vertical_wall(Map, R, R, C, NewMap) :-
    set_cell_safe(Map, coord(R,C), w, NewMap).
vertical_wall(Map, R1, R2,C, NewMap) :-
    R1 < R2 , R3 is R1+1,
    set_cell_safe(Map, coord(R1,C), w, Map2),
    vertical_wall(Map2, R3, R2,C, NewMap).

horizontal_split(Map, R, C1, C2, NewMap) :-
    horizontal_wall(Map,R,C1,C2,Map2),
    random_between(C1,C2,C3),
	Before is R-1, After is R+1,
	optional_set(Map2, f, Before, C3, Map3),
	optional_set(Map3, f, After, C3, Map4),
    set_cell(Map4, coord(R,C3), f, NewMap).

optional_set(Map, V, R, C, NewMap) :- 
	length(Map, RMax), R >= 0, R < RMax,
	Map = [Row|_],
	length(Row, CMax), C >= 0, C < CMax,!,
	set_cell(Map, coord(R,C), V, NewMap).
optional_set(Map, _, _, _, Map).

vertical_split(Map, R1, R2, C, NewMap) :-
    vertical_wall(Map,R1,R2,C,Map2),
    random_between(R1,R2,R3),
	Before is C-1, After is C+1,
	optional_set(Map2, f, R3, Before, Map3),
	optional_set(Map3, f, R3, After, Map4),
    set_cell(Map4, coord(R3,C), f, NewMap).

clean_map([],[]).
clean_map([Row|T], [CleanRow|T2]) :-
	clean_row(Row,CleanRow),
	clean_map(T,T2).

clean_row([],[]).
clean_row([n|T],[f|T2]) :- clean_row(T,T2).
clean_row([H|T],[H|T2]) :-
	H\=n,
	clean_row(T,T2).

find_value([],_,_,_,Acc,Acc).
find_value([Row|T], V, R,C, Acc, Coord) :-
    find_value_row(Row, V, R, C, Acc, Acc2),
    R2 is R+1,
    find_value(T,V,R2,0,Acc2,Coord).

find_value_row([], _, _, _, Acc, Acc).
find_value_row([V|T],V,R,C,Acc,Coord) :-
    C2 is C+1,
    find_value_row(T,V,R,C2,[coord(R,C)|Acc],Coord).
find_value_row([H|T],V,R,C,Acc,Coord) :-
    H\=V,C2 is C+1,
    find_value_row(T,V,R,C2,Acc,Coord).

gen_map(N, Rows, Cols, Map) :-
    make_base(Rows,Cols,Map1),
    R2 is Rows-1, C2 is Cols-1,
    gen_map(N, Map1, 0, R2, 0, C2, Map2),
	clean_map(Map2,Map3),
    find_value(Map3, f, 0, 0, [], Coord),
    random_permutation(Coord, [Start,Exit|_]),
    set_cell(Map3,Start,s,Map4),
    set_cell(Map4,Exit,e,Map).

gen_map(0, Map, _, _, _, _, Map).
gen_map(_, Map, R1,R2,C1,C2,Map) :- R2 < R1+2,C2 < C1+2.
gen_map(N, Map, R1,R2,C1,C2,NewMap) :-
    Height is R2-R1+1, Width is C2-C1+1,
    Width >= Height,!, N2 is N-1,
    C3 is C1+1, C4 is C2-1,
    random_between(C3,C4,C),
    vertical_split(Map, R1, R2, C, Map2),
    C2_2 is C-1, C1_2 is C+1,
    gen_map(N2, Map2, R1,R2,C1,C2_2,Map3),
    gen_map(N2, Map3, R1,R2,C1_2,C2,NewMap).
gen_map(N, Map, R1,R2,C1,C2,NewMap) :-
    N2 is N-1,
    R3 is R1+1, R4 is R2-1,
    random_between(R3,R4,R),
    horizontal_split(Map, R, C1, C2, Map2),
    R2_2 is R-1, R1_2 is R+1,
    gen_map(N2, Map2, R1,R2_2,C1,C2,Map3),
    gen_map(N2, Map3, R1_2,R2,C1,C2,NewMap).

display_map(Map) :-
    Map = [Row|_], length(Row,L),
    write('    ▐'),display_line(L,'▁▁'),write('▍'),nl,
    display_rows(Map),
    write('    ▐'),display_line(L,'▔▔'),write('▍'),nl.

display_rows([]).
display_rows([Row|T]) :-
    write('    ▐'),
    display_row(Row),
    write('▍'),nl,
    display_rows(T).

display_row([]).
display_row([H|T]) :- display_symbol(H), display_row(T).

display_symbol(n) :- write('░░'), !.
display_symbol(f) :- write('  '), !.
display_symbol(w) :- write('██'), !.
display_symbol(X) :- write(X), write(' ').

display_line(0,_).
display_line(N,V) :- N>0, N2 is N-1, write(V), display_line(N2,V).

show_random_map(N,R,C) :- gen_map(N,R,C,M),display_map(M).
