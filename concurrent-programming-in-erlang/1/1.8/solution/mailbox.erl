-module(mailbox).
-author("Santiago Botta <santiago@camba.coop>").
-export([start/0]).

%% @doc Starts a mailbox process
start() ->
  start([]).

%% @doc Given an array of messages, processes them in order of appearance .
-spec start([any()]) -> any().
start([]) ->
  receive
    FirstMessage ->
      io:format("Processing... ~p~n", [FirstMessage]),
      {ok, FirstMessage},
      start([])
  end;
start([X|Xs]) ->
  receive
    X ->
      io:format("Processing... ~p~n", [X]),
      {ok, X},
      start(Xs);
    Message ->
      start([X|Xs] ++ [Message])
  end.

