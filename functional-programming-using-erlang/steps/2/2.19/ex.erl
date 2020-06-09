-module(ex).
-author("@sgobotta").
-export([
  take/2, test_take/0,
  nub/1, test_nub/0,
  contains/2, test_contains/0,
  palindrome/1, test_palindrome/0
]).

%% @doc Given a number N and a list, takes the first N elements from the list
%% using tail recursion.
%% N should be less or equal the length of the Xs.
-spec take(float(), [T]) -> [T].
take(N, Xs) -> take(N, Xs, []).

take(0, _Xs, Accumulator) -> Accumulator;
take(_N, [], Accumulator) -> Accumulator;
take(N, [X|Xs], Accumulator) -> take(N-1, Xs, Accumulator ++ [X]).

test_take() ->
  []         = take(0, [4,2,0]),
  [4]        = take(1, [4,2,0]),
  [2,4]      = take(2, [2,4,6]),
  [4,2,0]    = take(3, [4,2,0]),
  [4,2,0]    = take(7, [4,2,0]),
  ""         = take(0, "santiago"),
  "san"      = take(3, "santiago"),
  "santiago" = take(27, "santiago"),
  {passed, "take tests passed succesfully"}.

%% @doc Given an element and a list tells whether the element exists in the list
%% or not.
-spec contains(T, [T]) -> boolean().
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

%% @doc Given a list returns a new list without duplicates.
-spec nub([T]) -> [T].
nub([]) -> [];
nub([X|Xs]) ->
  case contains(X, Xs) of
    true -> nub(Xs);
    false -> [X | nub(Xs)]
  end.

test_nub() ->
  [1,2,3,4,5] = nub([1,2,3,1,2,3,1,2,3,4,5]),
  [1,2,3,4,5] = nub([5,4,3,2,1,2,3,1,2,3,1,2,3,4,5]),
  [1,2] = nub([1,2,1,2,1,2,1,2]),
  [420] = nub([420]),
  {passed, "nub tests passed succesfully."}.

%% @doc Given a list, returns all elements except the last one.
-spec init([T]) -> [T].
init([_Last]) -> [];
init([X|Xs]) -> [X | init(Xs)].

%% @doc Given a list decides whether the elements matches a palindrome.
-spec palindrome([_T]) -> boolean().
palindrome([]) -> true;
palindrome([_X]) -> true;
palindrome([X|Xs]) -> (X == lists:last(Xs)) and palindrome(init(Xs)).

test_palindrome() ->
  false = palindrome([1,0,1,0,0]),
  false = palindrome([1,0,1,0]),
  true  = palindrome([1,0,0,0,1]),
  true  = palindrome("sagas"),
  true  = palindrome("solos"),
  true  = palindrome("stats"),
  true  = palindrome("tenet"),
  true  = palindrome("wow"),
  {passed, "palindrome tests passed succesfully"}.
