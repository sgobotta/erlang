-module(second).

-import(first,[
    square/1,
    area/3,
    sum/3
]).

-export([
    hypotenuse/2,
    area/2,
    perimeter/2
]).

% given two sides of a tractangle triangle returns the size of the hypotenuse
hypotenuse(X,Y) ->
    math:sqrt(square(X) + square(Y)).

% given two sides of a triangle rectangle returns the area
area(X,Y) ->
    H = hypotenuse(X,Y),
    area(X,Y,H).

% given two sides of a tractangle triangle returns the perimeter
perimeter(X,Y) ->
    H = hypotenuse(X,Y),
    sum(X,Y,H).

