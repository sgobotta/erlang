-module(list).
-author("@sgobotta").
-export([
  product_dr/1, test_product_dr/0,
  product_tr/1, test_product_tr/0,
  maximum_dr/1, test_maximum_dr/0,
  maximum_tr/1, test_maximum_tr/0
]).

%% @doc Given a list of numbers returns the product of it's elements.
%% The case base should return 1 since it's the neutral element of the
%% multiplication or the `id`.
product_dr([]) -> 1;
product_dr([X|Xs]) -> X * product_dr(Xs).

test_product_dr() ->
  test_product(fun product_dr/1, "product_dr tests passed succesfully.").

%% @doc Given a list of numbers return the product of each one.
product_tr(Xs) ->product_tr(Xs, 1).

%% @doc Given a list of numbers deconstructs the list to return accumulated
%% product results.
product_tr([], Accumulator) -> Accumulator;
product_tr([X|Xs], Accumulator) -> product_tr(Xs, X * Accumulator).

test_product_tr() ->
  test_product(fun product_tr/1, "product_tr tests passed succesfully.").

%% @doc Given a function/1 that calculates the product of a list of numbers and
%% an assertion feedback string, expects the assertions to pass.
test_product(ProductFunction, Assertion) ->
  1   = ProductFunction([]),
  2   = ProductFunction([1,2]),
  6   = ProductFunction([1,2,3]),
  24  = ProductFunction([1,2,3,4]),
  120 = ProductFunction([1,2,3,4,5]),
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
