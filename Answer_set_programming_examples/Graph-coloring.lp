% Define vertices and edges
vertex(1..6).
edge(1,2). edge(2,3). edge(3,4). edge(4,5). edge(5,6). edge(6,1).

% Available colors
color(red). color(blue). color(green).

% Each vertex must have exactly one color
1 { colored(V,C) : color(C) } 1 :- vertex(V).

% Adjacent vertices cannot have same color
:- edge(X,Y), colored(X,C), colored(Y,C).

% Make edges symmetric
edge(X,Y) :- edge(Y,X).

#show colored/2.