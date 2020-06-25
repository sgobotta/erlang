-module(client).
-author("Santiago Botta <santiago@camba.coop>").
-export([start/1]).

%% Obsolete after server refactor

%% @doc Given a server id, receives requests to be forwarded to the given server.
%% Usage:
%% ServerPid = spawn(server, server, []).
%% Client = spawn(client, start, [ServerPid].
%% Client ! {send, {check, "Madam Im Adam"}, self()}.
%% flush().
start(ServerPid) ->
  receive
    {send, {check, String}, Pid} ->
      ServerPid ! {check, String, self()},
      receive
        Response -> Pid ! Response
      end,
      start(ServerPid);
    {exit, Pid} ->
      io:format("Exiting client...~n"),
      Pid ! ok
  end.
