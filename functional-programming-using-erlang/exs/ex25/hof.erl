-module(hof).
-author("@sgobotta").
-include_lib("eunit/include/eunit.hrl").
-export([doubleAll/1, evens/1, product/1]).
-export([zip/2, zip_using_hof/2]).
-export([zip_with/3, zip_with_using_hof/3]).

%% Define the functions doubleAll, evens, and product using the higher-order
%% functions lists:map, lists:filter and lists:foldr.
%%
%% doubleAll([]) -> [];
%% doubleAll([X|Xs]) ->
%%     [ 2*X | doubleAll(Xs) ].
%% 
%% evens([]) -> [];
%% evens([X|Xs]) when X rem 2 == 0 ->
%%     [X | evens(Xs) ];
%% evens([_|Xs]) ->
%%     evens(Xs).
%% 
%% product([]) -> 1;
%% product([X|Xs]) -> X * product(Xs).
%%

-spec doubleAll([integer()]) -> [integer()].
doubleAll(Xs) -> lists:map(fun (X) -> 2*X end, Xs).

doubleAll_test() ->
  ?assertEqual([], doubleAll([])),
  ?assertEqual([2], doubleAll([1])),
  ?assertEqual([2,4,6,8,10], doubleAll([1,2,3,4,5])).

-spec evens([integer()]) -> [integer()].
evens(Xs) -> lists:filter(fun (X) -> X rem 2 == 0 end, Xs).

evens_test() ->
  ?assertEqual([], evens([])),
  ?assertEqual([], evens([1])),
  ?assertEqual([2], evens([2])),
  ?assertEqual([2,4,6,8,10], evens([1,2,3,4,5,6,7,8,9,10])).

-spec product([integer()]) -> integer().
product(Xs) -> lists:foldr(fun (X, Acc) -> X * Acc end, 1, Xs).

product_test() ->
  ?assertEqual(1, product([])),
  ?assertEqual(1, product([1])),
  ?assertEqual(2, product([1,2])),
  ?assertEqual(6, product([1,2,3])),
  ?assertEqual(24, product([1,2,3,4])),
  ?assertEqual(120, product([1,2,3,4,5])).

%% Zipping
%%
%% a) Define a function zip/2 that “zips together” pairs of elements from two
%% lists like this:
%%
%% zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]
%%
%% where you can see that the elements from the longer list are lost.
%%
-spec zip([A], [B]) -> [{A,B}].
zip(_, [])          -> [];
zip([], _)          -> [];
zip([X|Xs], [Y|Ys]) -> [{X,Y} | zip(Xs, Ys)].

zip_test() -> zip_generic_test(fun zip/2).

zip_generic_test(ZipFunction) ->
  ?assertEqual([], ZipFunction([], [])),
  ?assertEqual([{1,2}], ZipFunction([1], [2])),
  ?assertEqual([{1,2}], ZipFunction([1], [2,4])),
  ?assertEqual([{1,2}], ZipFunction([1,3], [2])),
  ?assertEqual([{1,2}, {3,4}], ZipFunction([1,3], [2,4])),
  ?assertEqual([{1,2}, {3,4}], ZipFunction([1,3], [2,4,6,8])),
  ?assertEqual([{1,2}, {3,4}], ZipFunction([1,3,5,7], [2,4])).

%% b) Define a function zip_with/3 that “zips together” pairs of elements from two lists using the function in the first argument, like this:
%% 
%% zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]
%%
-spec zip_with(fun((A,B) -> C), [A], [B]) -> [C].
zip_with(_F, _Xs, [])       -> [];
zip_with(_F, [], _Ys)       -> [];
zip_with(F, [X|Xs], [Y|Ys]) -> [F(X,Y) | zip_with(F, Xs, Ys)].

zip_with_test() ->
  Sum = fun (X,Y) -> X+Y end,
  zip_with_generic_test(fun zip_with/3, Sum).

%% c) Re-define the function zip_with/3 using zip and lists:map.
%%
-spec zip_with_using_hof(fun(({A,B}) -> C), A, B) -> [C].
zip_with_using_hof(F, Xs, Ys) -> lists:map(F, zip(Xs, Ys)).

zip_with_using_hof_test() ->
  Sum = fun ({X,Y}) -> X+Y end,
  zip_with_generic_test(fun zip_with_using_hof/3, Sum).

zip_with_generic_test(ZipWithFunction, F) ->
  ?assertEqual([],    ZipWithFunction(F, [], [])),
  ?assertEqual([3],   ZipWithFunction(F, [1], [2])),
  ?assertEqual([3],   ZipWithFunction(F, [1], [2,4])),
  ?assertEqual([3],   ZipWithFunction(F, [1,3], [2])),
  ?assertEqual([3,7], ZipWithFunction(F, [1,3], [2,4])),
  ?assertEqual([3,7], ZipWithFunction(F, [1,3], [2,4,6,8])),
  ?assertEqual([3,7], ZipWithFunction(F, [1,3,5,7], [2,4])).

%% d) Re-define zip/2 using zip_with/3.
%%
-spec zip_using_hof([A], [B]) -> [{A,B}].
zip_using_hof(Xs, Ys) -> zip_with(fun (X,Y) -> {X,Y} end, Xs, Ys).

zip_using_hof_test() -> zip_generic_test(fun zip_using_hof/2).
