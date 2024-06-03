dim(Map, Height, Width) :-
    length(Map, Height),
    Map = [Row|_],
    length(Row, Width).

nth_element([X|_], 1, X) :- !.
nth_element([_|T], N, X) :- N > 1, N1 is N - 1, nth_element(T, N1, X).

wall(Map, R, C) :-
    nth_element(Map, R, Row),
    nth_element(Row, C, w).

start(Map, R, C) :-
    nth_element(Map, R, Row),
    nth_element(Row, C, s).

exit(Map, R, C) :-
    nth_element(Map, R, Row),
    nth_element(Row, C, e).

free(Map, R, C) :-
    wall(Map, R, C), !, fail.
free(_, _, _).

edges(Map, Edges) :-
    dim(Map, Height, Width),
    Width_minus_one is Width - 1,
    Height_minus_one is Height - 1,
    findall(
        edge(pos(R,C),pos(R_plus_one,C),down),
        (
            between(1, Height_minus_one, R),
            between(1, Width, C),
            R_plus_one is R + 1,
            free(Map, R, C),
            free(Map, R_plus_one, C)
        ),
        Down),
    findall(
        edge(Dst,Src,up),
        member(edge(Src,Dst,_), Down),
        Up
    ),
    findall(
        edge(pos(R,C),pos(R,C_plus_one),right),
        (
            between(1, Height, R),
            between(1, Width_minus_one, C),
            C_plus_one is C + 1,
            free(Map, R, C),
            free(Map, R, C_plus_one)
        ),
        Right),
    findall(
        edge(Dst,Src,left),
        member(edge(Src,Dst,_), Right),
        Left
    ),
    append(Up, Down, Temp1),
    append(Left, Right, Temp2),
    append(Temp1, Temp2, Edges).

add_edge([], edge(Src, Dst, Dir), [graph(Src, [(Dst, Dir)])]).
add_edge([graph(Src,Neighbors)|T], edge(Src, Dst, Dir), [graph(Src, [(Dst, Dir)|Neighbors])|T]) :- !.
add_edge([H|T], edge(Src, Dst, Dir), [H|T2]) :- add_edge(T, edge(Src, Dst, Dir), T2).

add_edges(Graph, [], Graph).
add_edges(Graph, [H|T], Graph3) :- add_edge(Graph, H, Graph2), add_edges(Graph2, T, Graph3).

starts(Map, Ans) :-
    dim(Map, Height, Width),
    findall(
        pos(R, C),
        (
            between(1, Height, R),
            between(1, Width, C),
            start(Map, R, C)
        ),
        Ans
    ).

exits(Map, Ans) :-
    dim(Map, Height, Width),
    findall(
        pos(R, C),
        (
            between(1, Height, R),
            between(1, Width, C),
            exit(Map, R, C)
        ),
        Ans
    ).

find_exit(Map,Path) :-
    findall(Length, (member(Row, Map), length(Row, Length)), Lengths), 
    sort(Lengths, Ans), length(Ans, 1), Ans = [L], L > 0, 
    starts(Map, Starts), [Start] = Starts, 
    exits(Map, Exits), length(Exits, NumExits), NumExits > 0,
    edges(Map, Edges), add_edges([], Edges, Graph),
    search(Start, Exits, Graph, Path).

empty(([], [])).
enqueue(X, (E, D), ([X|E], D)) :- !.
dequeue((E, [X|D]), X, (E, D)) :- !.
dequeue(([],[]), _, _) :- !, fail.
dequeue((E, []), X, Y) :- reverse(E, RE), dequeue(([], RE), X, Y), !.

enqueueAll(Queue, [], Queue).
enqueueAll(Queue1, [parent(H,_)|T], Queue3) :- enqueue(H, Queue1, Queue2), enqueueAll(Queue2, T, Queue3).

search(Start, Exits, Graph, Path) :-
    empty(Queue1),
    enqueue(Start, Queue1, Queue2),
    Parents = [parent(Start, end)],
    bfs(Exits, Graph, Queue2, Parents, Path).

bfs(Exits, Graph, Queue1, Parents, Path) :-
    dequeue(Queue1, Node, Queue2),
    bfs2(Exits, Graph, Queue2, Parents, Node, Path).

bfs2(Exits, Graph, Queue, Parents, Node, Path) :-
    member(Node, Exits),
    reversed_path(Node, Parents, ReversedPath),
    reverse(ReversedPath, Path),
    !.
bfs2(Exits, Graph, Queue1, Parents, Node, Path) :-
    member(graph(Node, Neighbors), Graph),
    findall(
        parent(Neighbor, track(Node, Dir)),
        (
            member((Neighbor, Dir), Neighbors),
            \+ member(parent(Neighbor, _), Parents)
        ),
        ValidNeighbors
    ),
    enqueueAll(Queue1, ValidNeighbors, Queue2),
    append(ValidNeighbors, Parents, Parents2),
    bfs(Exits, Graph, Queue2, Parents2, Path).

reversed_path(Node, Parents, []) :-
    member(parent(Node, end), Parents),
    !.

reversed_path(Node, Parents, Path) :-
    member(parent(Node, track(Parent, Dir)), Parents),
    reversed_path(Parent, Parents, Path2),
    Path = [Dir|Path2].
