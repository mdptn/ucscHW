#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file

% functions.pl
% Megan Nguyen
% mednguye@ucsc.edu
% CMPS 112 Fall 2016
% Assignment 4

:- initialization(main).


% matches the airport abbreviation with the airport information
match_airport(From, To, Name1, Name2, LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2) :-
        airport(From, Name1a, degmin(LatD1a, LatM1a), degmin(LonD1a, LonM1a)),
        airport(To, Name2a, degmin(LatD2a, LatM2a), degmin(LonD2a, LonM2a)),
        Name1 = Name1a,
        Name2 = Name2a,
        LatD1 = LatD1a,
        LatM1 = LatM1a,
        LonD1 = LonD1a,
        LonM1 = LonM1a,
        LatD2 = LatD2a,
        LatM2 = LatM2a,
        LonD2 = LonD2a,
        LonM2 = LonM2a.


% converts degrees, min to radians. 1 min = 0.0166667 deg, 1 deg = 0.0174533 rad
convert_to_radians(LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2, Lat1, Lon1, Lat2, Lon2) :-
        Lat1 is (LatD1 + (LatM1 * 0.0166667)) * 0.0174533,
        Lon1 is (LonD1 + (LonM1 * 0.0166667)) * 0.0174533,
        Lat2 is (LatD2 + (LatM2 * 0.0166667)) * 0.0174533,
        Lon2 is (LonD2 + (LonM2 * 0.0166667)) * 0.0174533.


% haversine function from functions.pl example, gets distance between airports in miles
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
        Dlon is Lon2 - Lon1,
        Dlat is Lat2 - Lat1,
        A is sin( Dlat / 2 ) ** 2 + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
        Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
        Distance is Dist * 3961.


% calculates the arrival time with planes flying 500 mi/hr.
% calculates duration in minutes and then adds & divides accordingly for arrival time
calculate_arrival_time(DepHour, DepMin, Distance, ArrHour, ArrMin) :-
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
        match_airport(X, Y, Name1, Name2, LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2),
        convert_to_radians(LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2, Lat1, Lon1, Lat2, Lon2),
        haversine_radians(Lat1, Lon1, Lat2, Lon2, Distance),
        calculate_arrival_time(HourB, MinB, Distance, ArrHour, ArrMin),
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
        print(' '),
        print(ArrHour),
        print(':'),
        print(ArrMin),
        nl.



main :-
        [database].
