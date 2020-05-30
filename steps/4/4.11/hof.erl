-module(hof).
-author("sgobotta").
-include_lib("eunit/include/eunit.hrl").
-export([add/1,times/1,compose/2,id/1,iterate/1,compose/1,twice/1]).

add(X) ->
	fun(Y) -> X+Y end.

times(X) ->
	fun(Y) ->
		X*Y end.

compose(F,G) ->
	fun(X) -> G(F(X)) end.

-spec compose([function()]) -> function().
compose(Fs) ->
	lists:foldr(fun compose/2, fun id/1, Fs).

compose_test() ->
	Function = compose([add(2),add(2),add(2)]),
	?assertEqual(6, Function(0)).

twice(F) ->
	compose(F,F).

twice_test() ->
	?assertEqual(18, (twice(times(3)))(2)).

id(X) ->
	X.

-spec iterate(integer()) -> function().
iterate(0) ->
	fun id/1;
iterate(N) ->
	io:format("~p~n", [N]),
	fun (F) -> (iterate(N-1))(F) end.

iterate_test() ->
	?assertEqual(6, ((iterate(3))(add(2)))(0)).
