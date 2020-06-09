-module(exe2).

-export([area/1, perimeter/1, sumBit/1, test_sub_bit/0]).

perimeter({S, N}) -> perimeter(S, N).

perimeter(triangle, {X, Y, Z}) -> X + Y + Z.

area({S, N}) -> area(S, N).

area(triangle, {X, Y, Z}) ->
S = (X + Y + Z) div 2,
math:sqrt(S * (S - X) * (S - Y) * (S - Z)).

sumBit(0) -> 0;
sumBit(N) when N rem 2 == 0 -> 0 + sumBit(N div 2);
sumBit(N) -> 1 + sumBit(N div 2).

test_sub_bit() ->
  0 = sumBit(0),
  1 = sumBit(1),
  1 = sumBit(2),
  2 = sumBit(3),
  {passed, "tests passed succesfully"}.