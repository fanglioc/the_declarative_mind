:- use_module(library(clpfd)).

/* 1. N-Queens Problem */
n_queens(N, Queens) :-
    length(Queens, N),
    Queens ins 1..N,
    safe_queens(Queens),
    labeling([], Queens).

safe_queens([]).
safe_queens([Q|Queens]) :-
    safe_queens(Queens, Q, 1),
    safe_queens(Queens).

safe_queens([], _, _).
safe_queens([Q|Queens], Q0, D0) :-
    Q0 #\= Q,
    abs(Q0 - Q) #\= D0,
    D1 #= D0 + 1,
    safe_queens(Queens, Q0, D1).

/* 2. Sudoku Solver */
sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    blocks(R1,R2,R3), blocks(R4,R5,R6), blocks(R7,R8,R9),
    labeling([], Vs).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),
    blocks(Bs1, Bs2, Bs3).

/* 3. Map Coloring Problem */
map_coloring(Colors) :-
    Colors = [WA, NT, SA, Q, NSW, V, T],
    Colors ins 1..4,
    WA #\= NT, WA #\= SA,
    NT #\= SA, NT #\= Q,
    SA #\= Q, SA #\= NSW, SA #\= V,
    Q #\= NSW,
    NSW #\= V,
    labeling([], Colors).

/* 4. Scheduling Problem */
schedule_tasks(Tasks, MaxTime) :-
    Tasks = [Task1, Task2, Task3, Task4],
    Tasks ins 0..MaxTime,
    
    % Task durations
    Duration1 #= 2,
    Duration2 #= 3,
    Duration3 #= 4,
    Duration4 #= 2,
    
    % Ensure tasks complete within max time
    Task1 + Duration1 #=< MaxTime,
    Task2 + Duration2 #=< MaxTime,
    Task3 + Duration3 #=< MaxTime,
    Task4 + Duration4 #=< MaxTime,
    
    % Task dependencies
    Task2 #>= Task1 + Duration1,
    Task4 #>= Task2 + Duration2,
    Task4 #>= Task3 + Duration3,
    
    % Resource constraints
    nonoverlap([task(Task1,Duration1,1),
                task(Task2,Duration2,1),
                task(Task3,Duration3,1),
                task(Task4,Duration4,1)]),
    
    labeling([minimize(Task4)], Tasks).

/* 5. Knapsack Problem */
knapsack(Items, Capacity, Selection) :-
    length(Items, N),
    length(Selection, N),
    Selection ins 0..1,
    
    % Calculate total weight and value
    weights_values(Items, Weights, Values),
    scalar_product(Weights, Selection, #=<, Capacity),
    scalar_product(Values, Selection, #=, TotalValue),
    
    labeling([maximize(TotalValue)], Selection).

weights_values([], [], []).
weights_values([(W,V)|Items], [W|Weights], [V|Values]) :-
    weights_values(Items, Weights, Values).

/* Helper predicates */
print_solution(Name, Solution) :-
    write(Name), write(': '), write(Solution), nl.

print_sudoku([]).
print_sudoku([Row|Rows]) :-
    write(Row), nl,
    print_sudoku(Rows).

print_schedule(Tasks) :-
    Tasks = [T1,T2,T3,T4],
    write('Task 1 starts at: '), write(T1), nl,
    write('Task 2 starts at: '), write(T2), nl,
    write('Task 3 starts at: '), write(T3), nl,
    write('Task 4 starts at: '), write(T4), nl.

/* Example usage:
?- n_queens(8, Queens), print_solution('8-Queens', Queens