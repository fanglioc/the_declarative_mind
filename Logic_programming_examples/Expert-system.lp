/* Computer Problem Diagnosis Expert System */

/* Facts about symptoms and their related problems */
symptom(wont_boot, 'Computer won\'t start').
symptom(blue_screen, 'Blue screen error').
symptom(slow_performance, 'System is running slowly').
symptom(strange_noise, 'Strange noise from computer').
symptom(no_internet, 'No internet connection').
symptom(overheating, 'Computer feels hot').
symptom(program_crash, 'Programs keep crashing').
symptom(disk_full, 'Hard drive full').

/* Rules for diagnosis */
diagnosis(Problem, Cause, Solution) :-
    problem(Problem, Cause),
    solution(Problem, Solution).

/* Problem causes */
problem(wont_boot, 'Power supply failure') :-
    verify(no_power_lights),
    verify(no_fan_noise).

problem(wont_boot, 'RAM issue') :-
    verify(beeping_sound),
    verify(screen_remains_black).

problem(blue_screen, 'Driver conflict') :-
    verify(recent_hardware_change),
    verify(system_restarts).

problem(blue_screen, 'Hardware failure') :-
    verify(random_crashes),
    verify(system_unstable).

problem(slow_performance, 'Malware infection') :-
    verify(unexpected_popups),
    verify(high_cpu_usage).

problem(slow_performance, 'Low memory') :-
    verify(many_programs_running),
    verify(system_sluggish).

problem(strange_noise, 'Fan problem') :-
    verify(noise_from_fan_area),
    verify(temperature_increased).

problem(strange_noise, 'Hard drive failure') :-
    verify(clicking_sound),
    verify(slow_file_access).

/* Solutions */
solution(wont_boot, 'Replace power supply unit') :-
    problem(wont_boot, 'Power supply failure').

solution(wont_boot, 'Check and reseat RAM modules') :-
    problem(wont_boot, 'RAM issue').

solution(blue_screen, 'Roll back recent driver updates') :-
    problem(blue_screen, 'Driver conflict').

solution(blue_screen, 'Run hardware diagnostics') :-
    problem(blue_screen, 'Hardware failure').

solution(slow_performance, 'Run antivirus scan') :-
    problem(slow_performance, 'Malware infection').

solution(slow_performance, 'Upgrade RAM or close programs') :-
    problem(slow_performance, 'Low memory').

/* User interface predicates */
:- dynamic(fact/1).

verify(Question) :-
    fact(Question), !.

verify(Question) :-
    \+ fact(Question),
    ask_question(Question, Answer),
    Answer = yes,
    assertz(fact(Question)).

ask_question(Question, Answer) :-
    format('~w? (yes/no): ', [Question]),
    read(Answer).

/* Clear facts between consultations */
clear_facts :-
    retractall(fact(_)).

/* Main diagnostic procedure */
diagnose :-
    write('Computer Problem Diagnosis System'), nl,
    write('Please answer the following questions:'), nl,
    find_problem.

find_problem :-
    symptom(Problem, Description),
    write('Checking for: '), write(Description), nl,
    diagnosis(Problem, Cause, Solution),
    nl, write('Diagnosis complete:'), nl,
    write('Problem: '), write(Description), nl,
    write('Cause: '), write(Cause), nl,
    write('Recommended Solution: '), write(Solution), nl,
    clear_facts.

/* Example usage:
?- diagnose.
*/