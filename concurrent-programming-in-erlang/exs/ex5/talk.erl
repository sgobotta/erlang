-module(talk).
-export([worker/0]).

worker() ->
  process_flag(trap_exit, true),
  work(0).

work(N) ->
  receive
    {'EXIT', _Pid, Reason} ->
      io:format("<Talk> EXIT reason '~w'~n", [Reason]),
      io:format("<Talk> State is '~w'~n", [N])
  after 0 ->
    Msg = {self(), N},
    echo ! Msg,
    io:format("~w sent.~n",[Msg]),
    receive
      _Reply -> 
        timer:sleep(500),
        work(N+1)
    end
  end.
