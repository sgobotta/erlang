-module(utils).
-author("santiago@camba.coop").
-include_lib("eunit/include/eunit.hrl").
-export([least_frequents/1, most_frequents/1,take/2]).

least_frequents(Xs) -> modes(Xs, fun minimum/1).

most_frequents(Xs) -> modes(Xs, fun maximum/1).

%% @doc Given a non empty list of elements, and a function that calculates
%% a maximum or minimum number, returns the least or more frequent ones.
modes(Xs, F) ->
  Occurrences = lists:map(fun (X) -> occurrences(X, Xs) end, Xs),
  MaxOccurrence = F(Occurrences),
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

%% @doc Given a tuple, an accumulator and a value chooses whether the tuple
%% value should be added or not.
mode({Key, Value}, Accumulator, Value) -> Accumulator ++ [Key];
mode({_Key, _Value}, Accumulator, _MaxOccurrence) -> Accumulator.

modes_most_frequent_element_test() ->
  Maximum = fun maximum/1,
  ?assertEqual([rock],       modes([rock,paper,paper,rock,rock,scissors], Maximum)),
  ?assertEqual([rock],       modes([rock,paper,paper,rock,rock,scissors], Maximum)),
  ?assertEqual([paper,rock], modes([rock,scissors,rock,rock,paper,paper,paper], Maximum)),
  ?assertEqual([paper],      modes([paper,rock,rock,scissors,paper,paper,rock,paper], Maximum)),
  ?assertEqual([rock],       modes([rock], Maximum)).

modes_least_frequent_element_test() ->
  Minimum = fun minimum/1,
  ?assertEqual([scissors],        modes([rock,paper,paper,rock,rock,scissors], Minimum)),
  ?assertEqual([scissors],        modes([rock,paper,paper,rock,rock,scissors], Minimum)),
  ?assertEqual([paper,scissors],  modes([rock,scissors,rock,rock,rock,rock,paper], Minimum)),
  ?assertEqual([scissors],        modes([paper,rock,rock,scissors,paper,paper,rock,paper], Minimum)),
  ?assertEqual([rock],            modes([rock], Minimum)).

%% @doc Given an element and a list returns the number of occurences of the
%% element in the given list.
occurrences(X, Xs) -> occurrences(X, Xs, 0).

%% @doc Given an element, a list of elements and an accumulator, deconstructs
%% a list while accumulating occurrences of the given element.
occurrences(_X, [], Occurrences) -> Occurrences;
occurrences(X, [X|Xs], Occurrences) -> occurrences(X, Xs, 1 + Occurrences);
occurrences(X, [_Y|Xs], Occurrences) -> occurrences(X, Xs, Occurrences).

occurrences_test() ->
  ?assertEqual(0, occurrences(rock, [paper,paper,paper,scissors,paper])),
  ?assertEqual(1, occurrences(rock, [paper,paper,paper,scissors,rock])),
  ?assertEqual(1, occurrences(rock, [paper,paper,rock,scissors,paper])),
  ?assertEqual(1, occurrences(rock, [rock,paper,paper,scissors,paper])),
  ?assertEqual(5, occurrences(rock, [rock,rock,rock,paper,rock,rock])).

%% @doc Given a non empty list of numbers returns the maximum using tail
%% recursion.
maximum([X|Xs]) -> maximum(Xs, X).

maximum([], Max) -> Max;
maximum([X|Xs], Max) -> maximum(Xs, max(Max, X)).

%% @doc Given a non empty list of numbers returns the minimum using tail
%% recursion.
minimum([X|Xs]) -> minimum(Xs, X).

minimum([], Max) -> Max;
minimum([X|Xs], Max) -> minimum(Xs, min(Max, X)).

%% @doc Given a number of elements and a list, returns a list with the first N
%% elements.
take(0,_Xs) ->
    [];
take(_N,[]) ->
    [];
take(N,[X|Xs]) when N>0 ->
    [X|take(N-1,Xs)].

take_test() ->
  ?assertEqual([],      take(0, [1,2,3])),
  ?assertEqual([1],     take(1, [1,2,3])),
  ?assertEqual([1,2],   take(2, [1,2,3])),
  ?assertEqual([1,2,3], take(3, [1,2,3])),
  ?assertEqual([1,2,3], take(4, [1,2,3])).
