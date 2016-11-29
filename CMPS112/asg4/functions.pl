#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file

% functions.pl
% Megan Nguyen
% mednguye@ucsc.edu
% CMPS 112 Fall 2016
% Assignment 4

:- initialization(main).



not( X ) :- X, !, fail.
not( _ ).


% checks if an element is a member of a list. used to prevent backtracking


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


% if the start destination is the same as the end, zero-fly.
fly(X, X) :-
        print('Error: Zero-fly query - You are already at your destination.'),
        nl,
        halt.


fly(From, To) :-
        CFrom = From,
        get_departure_time(From, To, time(0, 0), P, CFrom, Visited),
        print(P).


% finds direct flights between locations
get_departure_time(X, Y, time(HourA, MinA), [X,Y], CFrom, Visited) :-
        flight(X, Y, time(HourB, MinB)),

        A is HourA + MinA/60,
        B is HourB + MinB/60,
        A < B,

        % call other functions to calculate travel time
        match_airport(X, Y, Name1, Name2, LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2),
        convert_to_radians(LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2, Lat1, Lon1, Lat2, Lon2),
        haversine_radians(Lat1, Lon1, Lat2, Lon2, Distance),
        calculate_arrival_time(HourB, MinB, Distance, RoundedMin, ArrHour, ArrMin),

        % print output
        format('depart ~a ~a ~d:~d ~n', [X, Name1, HourB, MinB]),
        format('arrive ~a ~a ~d:~d ~n', [Y, Name2, ArrHour, ArrMin]).


% finds the transfer flights
get_departure_time(X,Y, time(HourA, MinA), [X|Xs], CFrom, Visted) :-
        % make sure that there is no backtracking
        X \== Y,

        flight(X, W, time(HourB, MinB)),
        %not(W == CFrom),
        %not(member(W, NewVisit)),

        A is HourA + MinA/60,
        B is HourB + MinB/60,
        A < B,

        % call other functions to calculate travel time
        match_airport(X, W, Name1, Name2, LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2),
        convert_to_radians(LatD1, LatM1, LonD1, LonM1, LatD2, LatM2, LonD2, LonM2, Lat1, Lon1, Lat2, Lon2),
        haversine_radians(Lat1, Lon1, Lat2, Lon2, Distance),
        calculate_arrival_time(HourB, MinB, Distance, RoundedMin, ArrHour, ArrMin),

        DecimalArrival is ArrHour + (ArrMin / 60),

        % Cannot arrive past midnight
        DecimalArrival =< 24,

        % flight transfers always take 30 minutes
        NewTime is RoundedMin + 30,
        NewHour is NewTime // 60,
        NewMin is mod(NewTime, 60),


        % print output
        %format('depart ~a ~a ~d:~d ~n', [X, Name1, HourB, MinB]),
        %format('arrive ~a ~a ~d:~d ~n', [W, Name2, ArrHour, ArrMin]),

        get_departure_time(W, Y, time(NewHour, NewMin), Xs, CFrom, [X|Visited]).


main :-
        [database].
