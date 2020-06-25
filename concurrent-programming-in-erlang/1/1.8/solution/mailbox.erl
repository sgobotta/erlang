-module(mailbox).
-author("Santiago Botta <santiago@camba.coop>").
-include_lib("eunit/include/eunit.hrl").
-export([start/0, mailbox/1]).
-export([test_bed/0]).

test_bed() ->
  MailboxPid = mailbox:start(),
  MailboxPid ! "Handle me!".


%% @doc Starts a mailbox process
%% @end
start() ->
  io:format("Initialising mailbox...~n"),
  spawn(?MODULE, mailbox, [[]]).

%% @doc Given an array of messages, processes them in order of appearance.
%% @end
-spec mailbox([any()]) -> any().
mailbox([]) ->
  receive
    {Pid, FirstRequest} ->
      io:format("Processing... ~p~n", [FirstRequest]),
      Pid ! {ok, FirstRequest},
      mailbox([])
  end;
mailbox([{Pid, Request}|Xs]) ->
  receive
    stop ->
      ok;
    {Pid, _} ->
      io:format("Processing... ~p~n", [Request]),
      {ok, Request},
      mailbox(Xs);
    {AnotherPid, AnotherRequest} ->
      mailbox([Pid|Xs] ++ [AnotherPid, AnotherRequest])
  end.

