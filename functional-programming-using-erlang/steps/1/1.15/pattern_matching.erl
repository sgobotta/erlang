% @doc Pattern Mathing module for the 1.15 step of the FutureLearn Erlang Course.
-module(pattern_matching).

-export([
  xOr/2,
  xOr1/2,
  xOr2/2,
  xOr3/2,
  xOr4/2,
  max_three/3,
  how_many_equal/3
]).

% Exclusive Or from lesson

xOr(X,X) -> false;
xOr(_,_) -> true.

% Exclusive Or implementations

xOr1(true,true) -> false;
xOr1(false,false) -> false;
xOr1(_,_) -> true.

xOr2(true, false) -> true;
xOr2(false, true) -> true;
xOr2(_,_) -> false.

xOr3(X,Y) -> X =/= Y.

xOr4(X,Y) -> not X == Y.

% @doc Given three integers returns the maximum
max_three(X,Y,Z) -> max(X, max(Y,Z)).

% @doc Given three integers returns a number representing the equivalent ones
how_many_equal(X,X,X) -> 3;
how_many_equal(X,X,_) -> 2;
how_many_equal(X,_,X) -> 2;
how_many_equal(_,X,X) -> 2;
how_many_equal(_,_,_) -> 0.
