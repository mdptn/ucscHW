#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file

% functions.pl
% Megan Nguyen
% mednguye@ucsc.edu
% CMPS 112 Fall 2016
% Assignment 4

:- initialization(main).


fly(From, To) :-
        get_departure_time(From, To, time(0, 0)).



% if the start destination is the same as the end, zero-fly.
get_departure_time(X, X) :-
        print('Error: Zero-fly query - You are already at your destination.'),
        nl,
        halt.



% finds direct flights between locations
get_departure_time(X, Y, time(HourA, MinA)) :-
        flight(X, Y, time(HourB, MinB)),
        A is HourA + MinA/60,
        B is HourB + MinB/60,
        A < B,
        print(flight(X, Y, time(HourB, MinB))),
        nl.



main :-
        [database].
