-module(futures).

-export([]).

promise(Pid, Request) ->
  Tag = erlang:make_ref(),
  Pid ! {self(), Tag, Request},
  Tag.

yield(Tag) ->
  receive
    {Tag, Response} ->
      Response
    end.

% Tag = promise(Pid, fun() -> io:format("something...")),
% Val = yield(Tag).

pmap(L) ->
  S = self(),
  Pids = [do(S,F) || F <- L],
  [receive {Pid,Val} -> Val end || Pid <- Pids].

do(Parent, F) ->
  spawn(fun() ->
    Parent ! {self(), F()}
        end).
