-module(recursion).
-export([fib/1, fibs/1, pieces/1]).

%% @doc given an integer computes it's position in the fibonacci sequence
fib(0) -> 0;
fib(1) -> 1;
fib(X) -> fib(X-1) + fib(X-2).

%% @doc given a fibonacci sequence position returns the sum of fibonacci computations
fibs(X) -> 1 + 1 + fibs(X-1) + fibs(X-2).

%% @doc given a number of cuts returns the maximum number of pieces obtained
pieces(0) -> 1;
pieces(N) -> pieces(N-1) + N.
