/* Grammar Rules */
sentence(s(NP, VP)) -->
    noun_phrase(NP, Number),
    verb_phrase(VP, Number).

noun_phrase(np(Det, N), Number) -->
    determiner(Det, Number),
    noun(N, Number).

noun_phrase(np(PN), Number) -->
    proper_noun(PN, Number).

verb_phrase(vp(V, NP), Number) -->
    transitive_verb(V, Number),
    noun_phrase(NP, _).

verb_phrase(vp(V), Number) -->
    intransitive_verb(V, Number).

/* Lexicon */
determiner(det(the), _) --> [the].
determiner(det(a), singular) --> [a].
determiner(det(an), singular) --> [an].

noun(n(cat), singular) --> [cat].
noun(n(cats), plural) --> [cats].
noun(n(dog), singular) --> [dog].
noun(n(dogs), plural) --> [dogs].
noun(n(book), singular) --> [book].
noun(n(books), plural) --> [books].

proper_noun(pn(john), singular) --> [john].
proper_noun(pn(mary), singular) --> [mary].

transitive_verb(tv(sees), singular) --> [sees].
transitive_verb(tv(see), plural) --> [see].
transitive_verb(tv(likes), singular) --> [likes].
transitive_verb(tv(like), plural) --> [like].

intransitive_verb(iv(sleeps), singular) --> [sleeps].
intransitive_verb(iv(sleep), plural) --> [sleep].
intransitive_verb(iv(runs), singular) --> [runs].
intransitive_verb(iv(run), plural) --> [run].

/* Semantic Rules */
semantics(s(NP, VP), Meaning) :-
    np_semantics(NP, Subject),
    vp_semantics(VP, Subject, Meaning).

np_semantics(np(Det, N), Entity) :-
    det_semantics(Det, N, Entity).
np_semantics(np(PN), Entity) :-
    pn_semantics(PN, Entity).

vp_semantics(vp(V, NP), Subject, Meaning) :-
    tv_semantics(V, Relation),
    np_semantics(NP, Object),
    Meaning =.. [Relation, Subject, Object].
vp_semantics(vp(V), Subject, Meaning) :-
    iv_semantics(V, Relation),
    Meaning =.. [Relation, Subject].

/* Helper predicates for parsing */
parse(Sentence, Tree) :-
    phrase(sentence(Tree), Sentence).

parse_and_semantics(Sentence, Meaning) :-
    parse(Sentence, Tree),
    semantics(Tree, Meaning).

/* Pretty printing predicates */
print_tree(Tree) :-
    write_tree(Tree, 0).

write_tree(Term, Level) :-
    Term =.. [Functor|Args],
    tab(Level),
    write(Functor), nl,
    NextLevel is Level + 2,
    write_args(Args, NextLevel).

write_args([], _).
write_args([Arg|Args], Level) :-
    write_tree(Arg, Level),
    write_args(Args, Level).

/* Example usage:
?- parse([the, cat, sees, a, dog], Tree).
?- parse_and_semantics([john, likes, mary], Meaning).
?- parse([the, cats, sleep], Tree), print_tree(Tree).
*/

/* Test predicates */
test_parse(Sentence) :-
    parse(Sentence, Tree),
    write('Parse tree:'), nl,
    print_tree(Tree), nl,
    (parse_and_semantics(Sentence, Meaning) ->
        write('Semantic interpretation:'), nl,
        write(Meaning), nl
    ;
        write('No semantic interpretation available'), nl
    ).

test_all_parses(Sentence) :-
    findall(Tree, parse(Sentence, Tree), Trees),
    write('All possible parse trees:'), nl,
    print_trees(Trees).

print_trees([]).
print_trees([Tree|Trees]) :-
    print_tree(Tree), nl,
    print_trees(Trees).