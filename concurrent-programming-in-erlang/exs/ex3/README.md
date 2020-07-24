# Concurrent Programming using Erlang

## The Frequency Server

### Introduction

In this exercise we were introduced to the frequency server. A process that would let clients to allocate/deallocate frequencies. The server starts with a limited amount of frequencies to give. Every time a client sends a request, the server replies with a frequency response. The server would also have to determine if there are enough frequencies to allocate, or if a frequency was already allocated for a given client. It should also handle the case where there are no more frequencies to share.

### Starting the server

The `start/0` function is a wrapper for the `init/0` function, but also a helper that will let us call the process from a shell. At initialisation we'd want to register the process under a name. Erlang convention advices to use the name of the module, therefore we can just write `?MODULE`.
To make sure the process is properly registered, we should first create spawn the process, then register it and return the process `Pid` for further usage. That's all we need to know about the `start/0` function implementation. Both the spawning and the registration processes are performed inside the `start/0` function in a single step.

```erlang
init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).
```

```erlang
% We first point the shell in the exercise directory
1> cd("concurrent-programming-in-erlang/exs/ex3").
/home/sann/Documents/camba/capacitaciones/erlang/concurrent-programming-in-erlang/exs/ex3
ok
% Compile the module as usual
2> c(frequency).
{ok,frequency}
% And start the server
3> frequency:start().
<0.91.0>
% Here we request the server with an allocation message and get the frequency `10` returned with an ok signal.
4> frequency:allocate().
{ok,10}
% If we try to allocate again then we'll receive an tuple with an error
5> frequency:allocate().
{error,already_allocated}
% We try to deallocate and we receive an ok signal
6> frequency:deallocate(10).
ok
```

It's important to notice that we're no longer interacting with the server `Pid`, since the process is registered as `frequency`.
The `allocate/0` and `deallocate/1` messages are now part of the functional interface we had implemented to play the client role. It doesn't know anything about the server implementation and gets the work done with such a simple implementation.

### Stopping the server

This server ships with a stop interface that terminates the process and sends a signal to the request `Pid`.

```erlang
7> frequency:stop().
stopped
% Notice we're no longer capable of sending requests since the process was terminated
8> frequency:allocate().
** exception error: bad argument
     in function  frequency:allocate/0 (frequency.erl, line 51)
```

## Enhancing the Frequency Server

The server could not always reply with an answer at the time we expect it to. It may happen to be overloaded by a big number of requests made by other clients. We'd like the clients to handle that kind of situation where a server is not answering inmediately. We'd would add timeouts inside the client interfaces so that they're not kept waiting. We'd also clear the current client's mailbox whenever a function of the interface is used. For testing purposes it's useful to print those messages out so that we can verify those messages that have been queued due to server's delay.
A `set_overload` function was implemented to simulate an overload, where the server simply triggers a `timer:sleep/1` call

```erlang
Erlang/OTP 22 [erts-10.6] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.6  (abort with ^G)
1> cd("concurrent-programming-in-erlang/exs/ex3").
/home/.../ex3
ok
2> c(frequency).
{ok,frequency}
3> frequency:start().
<0.92.0>
% At this point we're simulating an overload, then the client wouldnt timeout after ?CLIENT_TIMEOUT (3000) milliseconds. Then we'll be ready to send `allocate/0` messages.
4> frequency:set_overload(10000).
{error,timeout}
5> frequency:allocate().
Cleared delivered message: {ok,0}
{error,timeout}
6> frequency:allocate().
Cleared delivered message: {ok,0}
{error,timeout}
7> frequency:allocate().
% It's possible to note the mailbox being flushed by the previous sent messages
Cleared Message: {reply,[83,101,114,118,101,114,32,111,118,101,114,108,111,97,100,101,100]}
Cleared Message: {reply,{ok,10}}
Cleared Message: {reply,{error,already_allocated}}
Cleared delivered message: {ok,3}
{error,already_allocated}
8> frequency:deallocate(10).
Cleared delivered message: {ok,0}
ok
```
