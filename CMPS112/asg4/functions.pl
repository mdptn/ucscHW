#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file

% functions.pl
% Megan Nguyen
% mednguye@ucsc.edu
% CMPS 112 Fall 2016
% Assignment 4

:- initialization(main).

hello :-
        write( 'Hello, World!' ), 
   nl.

main :-
        hello,
        halt.
