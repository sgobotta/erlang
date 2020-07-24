%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-author("Santiago Botta <santiago@camba.coop>").
-export([init/0]).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([set_overload/1]).

-define(CLIENT_TIMEOUT, 3000).

%% @doc Registers a frequency process to return a pid().
%% @end
-spec start() -> pid().
start() ->
  ServerPid = spawn(?MODULE, init, []),
  register(?MODULE, ServerPid),
  ServerPid.

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

%% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, {set_overload, Timeout}} ->
      ok = timer:sleep(Timeout),
      Pid ! {reply, "Server overloaded"},
      loop(Frequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keymember(Pid, 2, Allocated) of
    true ->
      {{[Freq|Free], Allocated}, {error, already_allocated}};
    _ ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq) ->
   case lists:keymember(Freq, 1, Allocated) of
    true ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free], NewAllocated};
    _ ->
      {Free, Allocated}
  end.

%% @doc Recursively flushes the mailbox while printing the ammount of
%%      cleared messages.
%% @end
clear() -> clear(0).
clear(ClearCount) ->
  receive
    Msg ->
      io:format("Cleared Message: ~w~n", [Msg]),
      clear(ClearCount + 1)
  after 0 -> {ok, ClearCount}
  end.

%% @doc Used in the client API to return a generic timeout error.
%% @end
timeout() -> {error, timeout}.

%% Functional interface

allocate() ->
  Cleared = clear(),
  io:format("Cleared delivered message: ~w~n", [Cleared]),
  frequency ! {request, self(), allocate},
  receive 
    {reply, Reply} -> Reply
  after ?CLIENT_TIMEOUT ->
    timeout()
  end.

deallocate(Freq) ->
  Cleared = clear(),
  io:format("Cleared delivered message: ~w~n", [Cleared]),
  frequency ! {request, self(), {deallocate, Freq}},
  receive 
    {reply, Reply} -> Reply
  after ?CLIENT_TIMEOUT ->
    timeout()
  end.

stop() -> 
  frequency ! {request, self(), stop},
  receive 
    {reply, Reply} -> Reply
  end.

%% @doc Asynchromously simulates and overload in the frequency server by adding
%%      a timeout of Milliseconds. This is only used for testing purposes.
%% @end
set_overload(Milliseconds) ->
  frequency ! {request, self(), {set_overload, Milliseconds}},
  receive
    {reply, Msg} -> io:format("~p~n", [Msg])
  after ?CLIENT_TIMEOUT ->
    timeout()
  end.
