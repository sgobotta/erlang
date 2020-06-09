-module(nub).
-author("@sgobotta").
-export([
  nub/1, test_nub/0,
  nub2/1, test_nub2/0,
  test_contains/0
]).

%% @doc Given an element and a list tells whether the element exists in the list
%% or not.
contains(_E, []) -> false;
contains(E, [E|_Xs]) -> true;
contains(E, [_X|Xs]) -> contains(E, Xs).

test_contains() ->
  false = contains(1, []),
  false = contains(1, [2]),
  false = contains(1, [3,0,0,0,6]),
  true  = contains(1, [1]),
  true  = contains(1, [1,2,3,4,5]),
  true  = contains(1, [2,3,4,5,1]),
  true  = contains(1, [3,4,1,5,6]),
  true  = contains(1, [3,1,1,1,6]),
  {passed, "contains tests passed succesfully."}.

%% @doc Given a list returns a new list without duplicates, preserving the
%% last occurrences.
-spec nub([T]) -> [T].
nub([]) -> [];
nub([X|Xs]) ->
  case contains(X, Xs) of
    true -> nub(Xs);
    false -> [X | nub(Xs)]
  end.

test_nub() ->
  [2,4,3,1] = nub([2,4,1,3,3,1]),
  [1,2,3,4,5] = nub([1,2,3,1,2,3,1,2,3,4,5]),
  [1,2,3,4,5] = nub([5,4,3,2,1,2,3,1,2,3,1,2,3,4,5]),
  [1,2] = nub([1,2,1,2,1,2,1,2]),
  [420] = nub([420]),
  {passed, "nub tests passed succesfully."}.

%% @doc Given a list returns a new list without duplicates, preserving the
%% occurrences order.
-spec nub2([T]) -> [T].
nub2(Xs) -> nub2(Xs, []).

-spec nub2([T], [T]) -> [T].
nub2([], Accumulator) -> Accumulator;
nub2([X|Xs], Accumulator) ->
  case contains(X, Accumulator) of
    true -> nub2(Xs, Accumulator);
    false -> nub2(Xs, Accumulator ++ [X])
  end.

test_nub2() ->
  [2,4,1,3] = nub2([2,4,1,3,3,1]),
  [4,2,0]   = nub2([4,2,0,0,2,4]),
  {passed, "nub2 tests passed succesfully"}.
