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
-export([test/0]).

-define(CLIENT_TIMEOUT, 3000).

test() ->
  %% Starts the server
  frequency:start(),
  %% Tries to allocate a frequency
  {ok, Frequency} = frequency:allocate(),
  %% When a frequency was already allocated, an error response is returned on
  %% an allocation attempt
  {error, already_allocated} = frequency:allocate(),
  %% Tries to deallocate a previously allocated frequency
  ok = frequency:deallocate(Frequency),
  %% When an unallocated frequency is attempted to be deallocated an error
  %% response is returned
  {error, unallocated_frequency} = frequency:deallocate(1000),
  %% Stops the server
  stopped = frequency:stop(),
  ok.
  

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
      try deallocate(Frequencies, Freq) of
        NewFrequencies -> Pid ! {reply, ok},
        loop(NewFrequencies)
      catch
        throw:unallocated_frequency ->
          Pid ! {reply, {error, unallocated_frequency}},
          loop(Frequencies)
      end;
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid), 
      loop(NewFrequencies)
  end.

%% Functional interface

command(Command) ->
  try frequency ! {request, self(), Command} of {request, _, Command} ->
    receive
      {reply, Result} -> Result;
      {error, Error} -> erlang:error(Error);
      {exit, Reason} -> erlang:exit(Reason)
    after(?CLIENT_TIMEOUT) ->
      erlang:error(timeout)
    end
  catch
    error:badarg ->
      erlang:error(server_down)
  end.


allocate() -> 
  command(allocate).

deallocate(Freq) -> 
  command({deallocate, Freq}).

stop() -> 
  command(stop).

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
  case lists:keysearch(Freq, 1, Allocated) of
    false ->
      throw(unallocated_frequency);
    {value,{Freq,Pid}} ->
      unlink(Pid),
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free], NewAllocated}
  end.

exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid,2,Allocated) of
    {value,{Freq,Pid}} ->
      NewAllocated = lists:keydelete(Freq,1,Allocated),
      {[Freq|Free],NewAllocated}; 
    false ->
      {Free,Allocated} 
  end.


