-module(first).

-export([
    double/1,
    mult/2,
    area/3,
    square/1,
    trbl/1,
    sum/3
]).


mult(X,Y) ->
    X*Y.

double(X) ->
    mult(2,X).

area(A,B,C) ->
    S = sum(A,B,C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).

square(X) ->
    mult(X,X).

trbl(X) ->
    mult(3,X).

sum(X,Y,Z) ->
    X+Y+Z.