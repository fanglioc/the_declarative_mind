% Courses and time slots
course(math101). course(physics101). course(cs101).
course(english101). course(history101).

timeslot(1..5).
room(r1). room(r2). room(r3).

% Course requirements
credits(math101,3). credits(physics101,4).
credits(cs101,3). credits(english101,3).
credits(history101,3).

prerequisite(physics101, math101).

% Schedule generation
{ scheduled(C,T,R) : timeslot(T), room(R) } 1 :- course(C).

% Constraints
:- scheduled(C1,T,R), scheduled(C2,T,R), C1 != C2.
:- scheduled(C1,T,R1), scheduled(C2,T,R2), C1 != C2.
:- scheduled(C1,T1,_), scheduled(C2,T2,_),
   prerequisite(C1,C2), T1 <= T2.
:- timeslot(T),
   #sum { Cr,C : scheduled(C,T,_), credits(C,Cr) } > 6.

#show scheduled/3.