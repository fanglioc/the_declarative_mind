/* Define the graph using edge/3 facts
   edge(From, To, Distance) */
edge(a, b, 4).
edge(a, c, 2).
edge(b, d, 3).
edge(c, d, 1).
edge(c, e, 5).
edge(d, e, 2).
edge(d, f, 6).
edge(e, f, 4).

/* Bidirectional edges */
connected(X, Y, D) :- edge(X, Y, D).
connected(X, Y, D) :- edge(Y, X, D).

/* Find a path between two nodes */
path(Start, End, Path, Distance) :-
    path(Start, End, [Start], Path, Distance).

path(End, End, Visited, Path, 0) :-
    reverse(Visited, Path).

path(Start, End, Visited, Path, Distance) :-
    connected(Start, Next, D),
    \+ member(Next, Visited),
    path(Next, End, [Next|Visited], Path, D1),
    Distance is D + D1.

/* Find shortest path using path length */
shortest_path(Start, End, Path, Distance) :-
    findall([P,D], path(Start, End, P, D), Paths),
    min_path(Paths, Path, Distance).

min_path([[Path,Dist]], Path, Dist).
min_path([[P1,D1],[_,D2]|Rest], MinPath, MinDist) :-
    D1 =< D2,
    min_path([[P1,D1]|Rest], MinPath, MinDist).
min_path([[_,D1],[P2,D2]|Rest], MinPath, MinDist) :-
    D1 > D2,
    min_path([[P2,D2]|Rest], MinPath, MinDist).

/* Find all paths with distance less than Max */
paths_under(Start, End, MaxDist, Path, Dist) :-
    path(Start, End, Path, Dist),
    Dist =< MaxDist.

/* Find paths visiting specific nodes */
path_through(Start, End, Through, Path, Distance) :-
    path(Start, Through, Path1, D1),
    path(Through, End, Path2, D2),
    append(P1, [_|P2], Path),
    append(Path1, Path2, Path),
    Distance is D1 + D2.

/* Helper predicates to print paths */
print_path([]).
print_path([H|T]) :-
    write(H), write(' -> '),
    print_path(T).

/* Example usage queries:
?- path(a, f, Path, Distance).
?- shortest_path(a, f, Path, Distance).
?- paths_under(a, f, 10, Path, Distance).
?- path_through(a, f, d, Path, Distance).
*/

/* Test predicates */
test_all_paths(Start, End) :-
    write('All paths from '), write(Start),
    write(' to '), write(End), write(':'), nl,
    path(Start, End, Path, Distance),
    write('Path: '), print_path(Path), nl,
    write('Distance: '), write(Distance), nl,
    fail.
test_all_paths(_, _).

test_shortest_path(Start, End) :-
    shortest_path(Start, End, Path, Distance),
    write('Shortest path from '), write(Start),
    write(' to '), write(End), write(':'), nl,
    write('Path: '), print_path(Path), nl,
    write('Distance: '), write(Distance), nl.