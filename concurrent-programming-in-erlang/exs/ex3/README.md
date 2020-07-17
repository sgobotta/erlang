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
