-module(client).
-author("Santiago Botta <santiago@camba.coop>").
-export([client/1]).

client(ServerPid) ->
  receive
    {send, {check, String}, Pid} ->
      ServerPid ! {check, String, self()},
      receive
        Response -> Pid ! Response
      end,
      client(ServerPid);
    {exit, Pid} ->
      io:format("Exiting client...~n"),
      Pid ! ok
  end.
