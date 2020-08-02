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
  catch exit(whereis(frequency), kill),
  timer:sleep(100),
  undefined = whereis(frequency),

  %% Starts the server
  frequency:start(),

  %% Tries to allocate a frequency
  {ok, Frequency} = frequency:allocate(),

  %% When a frequency was already allocated, an error response is returned on
  %% an allocation attempt
  {error, already_allocated} = frequency:allocate(),

  %% When an unallocated frequency is attempted to be deallocated an error
  %% is thrown
  try frequency:deallocate(Frequency + 1) of
    DeallocateResult ->
      erlang:error({unexpected_result, DeallocateResult})
  catch
    error:_M ->
      ok
  end,

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
    {request, From, stop} ->
      handle(stop, From, Frequencies);
    {request, From, Command} ->
      try handle(Command, From, Frequencies) of {Result, NewFrequencies} ->
        From ! {reply, Result},
        loop(NewFrequencies)
      catch
        throw:{Result, NewFrequencies} ->
          From ! {reply, Result},
          loop(NewFrequencies);
        Kind:Error:Stack ->
          io:format("Server ~p handling ~p: ~p~n\t~p~n", [Kind, Command, Error, Stack]),
          loop(Frequencies)
      end;
    {'EXIT', From, _Reason} ->
      NewFrequencies = exited(Frequencies, From),
      loop(NewFrequencies);
    UnknownMessage ->
      %% If we don't even know what process sent us this thing that we don't want
      %% we can only log an error and hope for the best...
      io:format("Unknown message received in Server: ~p~n", [UnknownMessage]),
      loop(Frequencies)
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

handle(allocate, From, Frequencies) ->
  allocate(Frequencies, From);
handle({deallocate, Freq}, _From, Frequencies) ->
  deallocate(Frequencies, Freq);
handle(stop, _From, Frequencies) ->
  {stopped, Frequencies}.

allocate({Freqs, Allocated}, Pid) ->
  case lists:keymember(Pid, 2, Allocated) of
    true -> {{error, already_allocated}, {Freqs, Allocated}};
    false -> do_allocate({Freqs, Allocated}, Pid)
  end.

do_allocate({[], Allocated}, _Pid) ->
  {{error, no_frequency}, {[], Allocated}};
do_allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  {{ok, Freq}, {Free, [{Freq, Pid}|Allocated]}}.

deallocate({Free, Allocated}, Freq) ->
  case lists:keysearch(Freq, 1, Allocated) of
    false ->
      erlang:error(unallocated_frequency);
    {value,{Freq,Pid}} ->
      unlink(Pid),
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {ok, {[Freq|Free], NewAllocated}}
  end.

exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid,2,Allocated) of
    {value,{Freq,Pid}} ->
      NewAllocated = lists:keydelete(Freq,1,Allocated),
      {[Freq|Free],NewAllocated}; 
    false ->
      {Free,Allocated} 
  end.
