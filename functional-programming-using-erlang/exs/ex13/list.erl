-module(list).
-author("@sgobotta").
-export([
  double_dr/1, test_double_dr/0,
  double_tr/1, test_double_tr/0,
  evens_dr/1, test_evens_dr/0,
  evens_tr/1, test_evens_tr/0,
  median/1, test_median/0,
  maximum_dr/1, test_maximum_dr/0,
  maximum_tr/1, test_maximum_tr/0,
  occurrences/2, test_occurrences/0,
  modes/1, test_modes/0,
  contains/2, test_contains/0,
  nub/1, test_nub/0
]).

%% @doc Given a list, returns all elements except the last one.
init([_Last]) -> [];
init([X|Xs]) -> [X | init(Xs)].

%% @doc Given a function/1 that calculates doubles each element of a list of
%% numbers and an assertion feedback string, expects the evaluations to pass.
test_double(DoubleFunction, Assertion) ->
  []                = DoubleFunction([]),
  [40,20,60,74,420] = DoubleFunction([20,10,30,37,210]),
  [2,4,6,8,10]      = DoubleFunction([1,2,3,4,5]),
  [420024]          = DoubleFunction([210012]),
  {passed, Assertion}.

%% @doc Given a list of numbers, returns a list with all it's elements doubled
%% using direct recursion.
double_dr([]) -> [];
double_dr([X|Xs]) -> [X*2 | double_dr(Xs)].

test_double_dr() ->
  test_double(fun double_dr/1, "double_dr tests passed succesfully.").

%% @doc Given a list of numbers, returns a list with all it's elements doubled
%% using tail recursion.
double_tr(Xs) -> double_tr(Xs,[]).

%% @doc Given a list and an Accumulator, deconstructs the list while appending
%% the double of the current list head to the end of the accumulator.
double_tr([], DoublesList) -> DoublesList;
double_tr([X|Xs], DoublesList) -> double_tr(Xs, DoublesList ++ [X*2]).

test_double_tr() ->
  test_double(fun double_tr/1, "double_tr tests passed succesfully.").

test_evens(EvensFunction, Assertion) ->
  []             = EvensFunction([]),
  []             = EvensFunction([1,3,5,7,9]),
  [0,2,4,6,8,10] = EvensFunction([0,1,2,3,4,5,6,7,8,9,10]),
  [10,8,6,4,2,0] = EvensFunction([10,9,8,7,6,5,4,3,2,1,0]),
  {passed, Assertion}.

%% @doc Given a list of numbers returns a filtered list with even numbers using
%% direct recursion.
evens_dr([]) -> [];
evens_dr([X|Xs]) when X rem 2 == 0 -> [X | evens_dr(Xs)];
evens_dr([_X|Xs]) -> evens_dr(Xs).

test_evens_dr() ->
  test_evens(fun evens_dr/1, "evens_dr tests passed succesfully").

%% @doc Given a list of numbers returns a filtered list with even numbers using
%% tail recursion.
evens_tr(Xs) -> evens_tr(Xs, []).

%% @doc Given a list and an accumulator, deconstructs the list while filtering
%% even numbers into the accumulator.
evens_tr([], FilteredList) -> FilteredList;
evens_tr([X|Xs], FilteredList) when X rem 2 == 0 ->
  evens_tr(Xs, FilteredList ++ [X]);
evens_tr([_X|Xs], FilteredList) -> evens_tr(Xs, FilteredList).

test_evens_tr() ->
  test_evens(fun evens_tr/1, "evens_dr tests passed succesfully").

%% @doc Given a non empty list of numbers, returns the median element using
%% direct recursion.
median(Xs) -> median_aux(lists:sort(Xs)).

%% @doc Given a sorted list returns the median element using direct recursion.
median_aux([X]) -> X;
median_aux([X,Y]) -> (X + Y) / 2;
median_aux(Xs) -> median_aux(tl(init(Xs))).

test_median() ->
  test_median(fun median/1, "median tests passed succesfully").

test_median(MedianFunction, Assertion) ->
  1    = MedianFunction([1]),
  1.5  = MedianFunction([1,2]),
  2    = MedianFunction([1,2,3]),
  2    = MedianFunction([3,2,1]),
  4    = MedianFunction([1,6,2,5,4]),
  25   = MedianFunction([10,15,30,25,56]),
  32.5 = MedianFunction([10,35,30,15,86,42]),
  {passed, Assertion}.


%% @doc Given a non empty list of numbers returns the maximum using direct
%% recursion.
maximum_dr([X]) -> X;
maximum_dr([X|Xs]) -> max(X, maximum_dr(Xs)).

test_maximum_dr() ->
  test_maximum(fun maximum_dr/1, "maximum_dr tests passed succesfully.").

%% @doc Given a non empty list of numbers returns the maximum using tail
%% recursion.
maximum_tr([X|Xs]) -> maximum_tr(Xs, X).

maximum_tr([], Max) -> Max;
maximum_tr([X|Xs], Max) -> maximum_tr(Xs, max(Max, X)).

test_maximum_tr() ->
  test_maximum(fun maximum_tr/1, "maximum_tr tests passed succesfully.").

%% @doc Given a function/1 that calculates the maximum of a list of numbers and
%% an assertion feedback string, expects the evaluations to pass.
test_maximum(MaximumFunction, Assertion) ->
1   = MaximumFunction([1]),
1   = MaximumFunction([0,1]),
1   = MaximumFunction([1,1]),
2   = MaximumFunction([2,1]),
3   = MaximumFunction([0,3,1]),
420 = MaximumFunction([0,13,6,141,420,419]),
{passed, Assertion}.

%% @doc Given an element and a list returns the number of occurences of the
%% element in the given list.
occurrences(X, Xs) -> occurrences(X, Xs, 0).

%% @doc Given an element, a list of elements and an accumulator, decontructs
%% a list while accumulating occurrences of the given element.
occurrences(_X, [], Occurrences) -> Occurrences;
occurrences(X, [X|Xs], Occurrences) -> occurrences(X, Xs, 1 + Occurrences);
occurrences(X, [_Y|Xs], Occurrences) -> occurrences(X, Xs, Occurrences).

test_occurrences() ->
  0 = occurrences(5, [1,2,3,4,3,2,3,4,3,2]),
  1 = occurrences(1, [1,2,3,4,3,2,3,4,3,2]),
  1 = occurrences(1, [5,2,3,4,1,2,3,4,3,2]),
  1 = occurrences(1, [5,2,3,4,5,2,3,4,3,1]),
  5 = occurrences(1, [1,2,1,4,5,1,1,4,3,1]),
  {passed, "occurrences tests succesfully pased"}.

%% @doc Given a non empty list of numbers, returns the most frequent numbers.
modes(Xs) ->
  Occurrences = lists:map(fun (X) -> occurrences(X, Xs) end, Xs),
  MaxOccurrence = maximum_dr(Occurrences),
  ElementOccurrences = maps:from_list(
    lists:zipwith(fun (X, Y) -> {X, Y} end, Xs, Occurrences)
  ),
  maps:fold(
    fun (Key, Value, Accumulator) ->
      mode({Key, Value}, Accumulator, MaxOccurrence)
    end,
    [],
    ElementOccurrences
  ).

%% @doc Given a tuple, an accumulator and a value decides whether the tuple
%% value should be added or not.
mode({Key, Value}, Accumulator, Value) -> Accumulator ++ [Key];
mode({_Key, _Value}, Accumulator, _MaxOccurrence) -> Accumulator.

test_modes() ->
  [1]   = modes([1,2,3,4,1,1,6,6,7]),
  [1,6] = modes([1,2,3,4,1,1,6,6,6,7]),
  [420] = modes([420,24,24,42,4,2,420,0,20,420,24,420,2,0]),
  [420] = modes([420]),
  {passed, "modes tests passed succesfully"}.

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

%% @doc Given a list returns a new list without duplicates.
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
