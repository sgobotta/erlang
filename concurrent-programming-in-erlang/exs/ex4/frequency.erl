%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(frequency,
    spawn(?MODULE, init, [])).

init() ->
  process_flag(trap_exit, true),
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
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
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid), 
      loop(NewFrequencies)
  end.

%% Functional interface

allocate() -> 
  frequency ! {request, self(), allocate},
  receive 
    {reply, Reply} -> Reply;
    _              -> ok
  end.

deallocate(Freq) -> 
  frequency ! {request, self(), {deallocate, Freq}},
  receive 
    {reply, Reply} -> Reply;
    _              -> ok
  end.

stop() -> 
  frequency ! {request, self(), stop},
  receive 
    {reply, Reply} -> Reply
  end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({Freqs, Allocated}, Pid) ->
  case lists:keymember(Pid, 2, Allocated) of
    true -> {{Freqs, Allocated}, {error, already_allocated}};
    false -> do_allocate({Freqs, Allocated}, Pid)
  end.

do_allocate({[], Allocated}, _Pid) ->
  {{[], Allocated},  {error, no_frequency}};
do_allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq, 1, Allocated),
  unlink(Pid),
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free], NewAllocated}.

exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid,2,Allocated) of
    {value,{Freq,Pid}} ->
      NewAllocated = lists:keydelete(Freq,1,Allocated),
      {[Freq|Free],NewAllocated}; 
    false ->
      {Free,Allocated} 
  end.

