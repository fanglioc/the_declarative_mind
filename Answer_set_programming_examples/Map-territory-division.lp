% Regions and adjacency
region(1..10).
adjacent(1,2). adjacent(2,3). adjacent(3,4).
adjacent(4,5). adjacent(5,1). adjacent(2,6).
adjacent(3,7). adjacent(4,8). adjacent(5,9).

% Population data
population(1,1000). population(2,1500).
population(3,800). population(4,1200).
population(5,900). population(6,700).
population(7,1100). population(8,1300).
population(9,950). population(10,850).

% Territory assignment
territory(a). territory(b). territory(c).

% Assignment rules
1 { assigned(R,T) : territory(T) } 1 :- region(R).

% Connected territories constraint
connected(T) :- territory(T),
    #count { R : assigned(R,T) } > 0.

% Population balance
:- territory(T),
   #sum { P,R : assigned(R,T), population(R,P) } > 4000.

% Territory cohesion
:- adjacent(R1,R2), assigned(R1,T1), assigned(R2,T2),
   T1 != T2, #count { R : assigned(R,T1) } < 3.

#show assigned/2.