-module(recursion).
-export([
  fib/1,
  test_fib/0,
  is_perfect_number/1,
  test_is_perfect_number/0
]).

fib(N) -> fib(N,0,1).

%% @doc fibonacci implementation using tail recursion
fib(0,X,_) -> X;
fib(N,X,Y) -> fib(N-1,X+Y,X).

test_fib() ->
  0  = fib(0),
  1  = fib(1),
  1  = fib(2),
  2  = fib(3),
  3  = fib(4),
  5  = fib(5),
  8  = fib(6),
  13 = fib(7),
  21 = fib(8),
  34 = fib(9),
  55 = fib(10),
  {passed, "k passed succesfully"}.

%% @doc given an integer tells whether it's a perfect number or not
is_perfect_number(N) -> is_perfect_number(N-1,N,0).

is_perfect_number(0,N,A) -> N == A;
is_perfect_number(X,N,A) when N rem X == 0 -> is_perfect_number(X-1,N,A+X);
is_perfect_number(X,N,A) -> is_perfect_number(X-1,N,A).

test_is_perfect_number() ->
  true  = is_perfect_number(6),
  true  = is_perfect_number(28),
  true  = is_perfect_number(496),
  true  = is_perfect_number(8128),
  false = is_perfect_number(7),
  false = is_perfect_number(14),
  false = is_perfect_number(248),
  false = is_perfect_number(6096),
  {passed, "perfect_number specs passed succesfully."}.
