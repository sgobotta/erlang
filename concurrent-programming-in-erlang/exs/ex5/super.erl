-module(super).
-export([super/0]).

super() ->
  process_flag(trap_exit, true),
  E = spawn_link(echo,listener,[]),
  register(echo,E),
  io:format("echo spawned.~n"),
  T = spawn_link(talk,worker,[]),
  register(talk,T),
  io:format("worked spawned as Pid ~w.~n",[whereis(talk)]),
  loop(E,T).

loop(E,T) ->
  receive
    {'EXIT', E, EchoReason} ->
      io:format("Echo EXIT reason '~w'~n", [EchoReason]),
      timer:sleep(2000),
      NewEcho = spawn_link(echo,listener,[]),
      register(echo, NewEcho),
      io:format("Echo respawned as '~w'~n", [NewEcho]),
      loop(NewEcho, T);
    {'EXIT', T, TalkReason} ->
      io:format("Talk EXIT reason '~w'~n", [TalkReason]),
      timer:sleep(2000),
      NewTalk = spawn_link(talk,worker,[]),
      register(talk, NewTalk),
      io:format("Talk respawned as '~w'~n", [NewTalk]),
      loop(E, NewTalk)
  end.
