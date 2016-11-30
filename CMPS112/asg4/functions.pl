#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file

% functions.pl
% Megan Nguyen
% mednguye@ucsc.edu
% CMPS 112 Fall 2016
% Assignment 4

:- initialization(main).


not( X ) :- X, !, fail.
not( _ ).


% checks if the airports entered are real and throws an errror if not
fly(Isnt, Real) :-
        not(match_airport(Isnt, Real, _, _, _)),
        print('Error: One or more airports specified are non-existent.'),
        nl,
        halt.


% if the start destination is the same as the end, zero-fly.
fly(X, X) :-
        print('Error: Zero-fly query - You are already at your destination.'),
        nl,
        halt.


% valid airports, proceed.
fly(From, To) :-
        get_departure_time(From, To, time(0, 0), [From], List),
        %print_flights(List),
        write(List).
        %split_flight_list(List).


% matches the airport abbreviation with the airport information
% gets distance between them too
% deg,min to radians. 1 min = 0.0166667 deg, 1 deg = 0.0174533 rad
match_airport(From, To, Name1, Name2, Distance) :-
        airport(From, Name1, degmin(LatD1, LatM1), degmin(LonD1, LonM1)),
        airport(To, Name2, degmin(LatD2, LatM2), degmin(LonD2, LonM2)),
        Lat1 is (LatD1 + (LatM1 * 0.0166667)) * 0.0174533,
        Lon1 is (LonD1 + (LonM1 * 0.0166667)) * 0.0174533,
        Lat2 is (LatD2 + (LatM2 * 0.0166667)) * 0.0174533,
        Lon2 is (LonD2 + (LonM2 * 0.0166667)) * 0.0174533,
        haversine_radians(Lat1, Lon1, Lat2, Lon2, Distance).


% haversine function from functions.pl example, gets distance between airports in miles
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
        Dlon is Lon2 - Lon1,
        Dlat is Lat2 - Lat1,
        A is sin( Dlat / 2 ) ** 2 + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
        Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
        Distance is Dist * 3961.


% calculates the arrival time with planes flying 500 mi/hr.
% calculates duration in minutes and then adds & divides accordingly for arrival time
calculate_arrival_time(DepHour, DepMin, Distance, RoundedMin, ArrHour, ArrMin) :-
        % convert departure time into minutes
        DecimalMin is DepMin / 60,
        DepartMin is (DepHour + DecimalMin) * 60,

        % find duration of flight in minutes
        DurationMin is Distance / 500 * 60,

        % add duration of flight to the departure time
        ArrivalMin is DepartMin + DurationMin,
        RoundedMin is truncate(ArrivalMin),
        ArrHour is RoundedMin // 60,
        ArrMin is mod(RoundedMin, 60).


get_departure_time(From, To, time(HourA, MinA), Visited, [Flight|List]) :-
        flight(From, To, time(HourB, MinB)),
        not(member(To, Visited)),

        A is HourA + MinA/60,
        B is HourB + MinB/60,
        % no flights depart past midnight and be at a possible time
        A =< 24,
        A =< B,

        match_airport(From, To, Name1, Name2, Distance),
        calculate_arrival_time(HourB, MinB, Distance, RoundedMin, ArrHour, ArrMin),

        %format('depart ~a ~a ~d:~d ~n', [From, Name1, HourB, MinB]),
        %format('arrive ~a ~a ~d:~d ~n', [To, Name2, ArrHour, ArrMin]),        

        atom_concat('depart ', From, String1),
        atom_concat(' ', Name1, String2),
        %atom_concat(' ', NewHB, String3),
        %atom_concat(':', MinB, String4),
        atom_concat(String1, String2, String5),
        %atom_concat(String5, String3, String6),
        %atom_concat(String6, String4, DString),

        atom_concat('arrive ', To, String7),
        atom_concat(' ', Name2, String8),
        %atom_concat(' ', ArrHour, String9),
        %atom_concat(':', ArrMin, String10),
        atom_concat(String7, String8, String11),
        %atom_concat(String11, String9, String12),
        %atom_concat(String12, String10, AString),

        % add info to flight list
        %Flight = [From, Name1, HourB, MinB, To, Name2, ArrHour, ArrMin]
        %Flight = [DString, AString].
        Flight = [String5, String11].


get_departure_time(From, To, time(HourA, MinA), Visited, [Flight|List]) :-
        flight(From, W, time(HourB, MinB)),
        % prevent backtracking
        not(W == To),
        not(member(W, Visited)),

        A is HourA + MinA/60,
        B is HourB + MinB/60,
        % no flights depart past midnight and be at a possible time
        A =< 24,    
        A =< B,

        match_airport(From, W, Name1, Name2, Distance),
        calculate_arrival_time(HourB, MinB, Distance, RoundedMin, ArrHour, ArrMin),

        atom_concat('depart ', From, String1),
        atom_concat(' ', Name1, String2),
        %atom_concat(' ', NewHB, String3),
        %atom_concat(':', MinB, String4),
        atom_concat(String1, String2, String5),
        %atom_concat(String5, String3, String6),
        %atom_concat(String6, String4, DString),

        atom_concat('arrive ', W, String7),
        atom_concat(' ', Name2, String8),
        %atom_concat(' ', ArrHour, String9),
        %atom_concat(':', ArrMin, String10),
        atom_concat(String7, String8, String11),
        %atom_concat(String11, String9, String12),
        %atom_concat(String12, String10, AString),
        
        % add info to flight list
        %Flight = [From, Name1, HourB, MinB, W, Name2, ArrHour, ArrMin],
        Flight = [String5, String11],

        % flight transfers always take 30 minutes
        NewTime is RoundedMin + 30,
        NewHour is NewTime // 60,
        NewMin is mod(NewTime, 60),

        get_departure_time(W, To, time(NewHour, NewMin), [W|Visited], List).


% recursive function that splits the head of the list and calls to print it
split_flight_list([]).
split_flight_list([H|T]) :-
        print_flight_list(H),
        nl,
        split_flight_list(T).



% prints the flight list in the desired format
print_flight_list([]).
print_flight_list(List) :-
        write(List).
        %format('depart ~a ~a ~d:~d ~n', [A,B,C,D]).
        %format('arrive ~a ~a ~d:~d ~n', [W, Name2, ArrHour, ArrMin]),

main :-
        [database].