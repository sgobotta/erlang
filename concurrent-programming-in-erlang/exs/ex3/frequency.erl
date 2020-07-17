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
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() -> 
  frequency ! {request, self(), allocate},
  receive 
    {reply, Reply} -> Reply
  end.

deallocate(Freq) -> 
  frequency ! {request, self(), {deallocate, Freq}},
  receive 
    {reply, Reply} -> Reply
  end.

stop() -> 
  frequency ! {request, self(), stop},
  receive 
    {reply, Reply} -> Reply
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
