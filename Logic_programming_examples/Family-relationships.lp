/* Family Relationships in Prolog */

/* Facts: parent(Parent, Child) */
parent(john, mary).
parent(john, ann).
parent(jane, mary).
parent(jane, ann).
parent(mary, peter).
parent(mary, paul).
parent(steve, peter).
parent(steve, paul).
parent(ann, james).
parent(ann, susan).
parent(bob, james).
parent(bob, susan).

/* Rules for different family relationships */

% Father relationship
father(Father, Child) :-
    parent(Father, Child),
    male(Father).

% Mother relationship
mother(Mother, Child) :-
    parent(Mother, Child),
    female(Mother).

% Gender facts
male(john).
male(steve).
male(bob).
male(peter).
male(paul).
male(james).

female(jane).
female(mary).
female(ann).
female(susan).

% Sibling relationship
sibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \= Y.

% Brother relationship
brother(X, Y) :-
    sibling(X, Y),
    male(X).

% Sister relationship
sister(X, Y) :-
    sibling(X, Y),
    female(X).

% Grandparent relationship
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).

% Ancestor relationship
ancestor(A, D) :-
    parent(A, D).
ancestor(A, D) :-
    parent(A, X),
    ancestor(X, D).

% Uncle/Aunt relationship
uncle_aunt(UA, NC) :-
    parent(P, NC),
    sibling(UA, P).

% Cousin relationship
cousin(X, Y) :-
    parent(P1, X),
    parent(P2, Y),
    sibling(P1, P2).

/* Example queries:
?- sibling(mary, ann).
?- grandparent(john, peter).
?- ancestor(jane, james).
?- cousin(peter, james).
?- uncle_aunt(ann, peter).
*/

/* Helper predicates for testing relationships */
test_relationships(Person) :-
    write('Parents of '), write(Person), write(':'), nl,
    parent(Parent, Person),
    write(Parent), nl,
    fail.
test_relationships(_).

test_siblings(Person) :-
    write('Siblings of '), write(Person), write(':'), nl,
    sibling(Sibling, Person),
    write(Sibling), nl,
    fail.
test_siblings(_).

test_grandparents(Person) :-
    write('Grandparents of '), write(Person), write(':'), nl,
    grandparent(GP, Person),
    write(GP), nl,
    fail.
test_grandparents(_).