% Users and relationships
user(1..10).
follows(1,2). follows(2,3). follows(3,4).
follows(4,1). follows(5,1). follows(6,2).
follows(7,3). follows(8,4). follows(9,5).

% User interests
interest(gaming). interest(music). interest(sports).
interest(books). interest(movies).

has_interest(1,gaming). has_interest(1,music).
has_interest(2,sports). has_interest(2,gaming).
has_interest(3,books). has_interest(3,movies).
has_interest(4,music). has_interest(4,sports).

% Community detection
{ community(U,C) : C=1..3 } 1 :- user(U).

% Similar interests identification
similar_interests(U1,U2) :- user(U1), user(U2),
    has_interest(U1,I), has_interest(U2,I),
    U1 != U2.

% Calculate community cohesion
cohesion_score(S) :- S = #count {
    U1,U2 : similar_interests(U1,U2),
            community(U1,C), community(U2,C)
}.

% Calculate user influence
influence_score(U,S) :- user(U),
    S = #count { V : follows(V,U) }.

% Friend recommendations
potential_friend(U1,U2) :- user(U1), user(U2),
    similar_interests(U1,U2),
    not follows(U1,U2), not follows(U2,U1),
    U1 != U2.

#maximize { S : cohesion_score(S) }.
#show community/2.
#show potential_friend/2.
#show influence_score/2.