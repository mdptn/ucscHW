#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file

% functions.pl
% Megan Nguyen
% mednguye@ucsc.edu
% CMPS 112 Fall 2016
% Assignment 4

:- initialization(main).


fly(From, To) :-
        get_departure_time(From, To, time(0, 0)).


match_airport(From, To, Name1, Name2) :-
        airport(From, Name1a, degmin(_, _), degmin(_, _)),
        airport(To, Name2a, degmin(_, _), degmin(_, _)),
        Name1 = Name1a,
        Name2 = Name2a.




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
        match_airport(X, Y, Name1, Name2),
        print('depart '),
        print(X),
        print(' '),
        print(Name1),
        print(' '),
        print(HourB),
        print(':'),
        print(MinB),
        nl,
        print('arrive '),
        print(Y),
        print(' '),
        print(Name2),
        nl.



main :-
        [database].
