-module(test).
-export([fac/1]).

fac(0) -> 1;
fac(N) -> fac(N-1)*N.
