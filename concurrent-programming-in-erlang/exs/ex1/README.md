# Concurrent Programming using Erlang

## Palindrome checker process

In this exercise we were encouraged to implemented a simple server that handles requests that check whether a string is a palindrome or not. Then we were presented with anotehr problem: what would happen if a server is overloaded with requests? Is there a way to handle requests from multiple isntances of the same server? There we came up with two different solutions. First an implementation of a simple server, then reuse the same server code to handle multiple requests.

### Simple single client server

At first I'd have to get in the erlang shell and:

0. compile the server module,
1. spawn a server process manually,
2. send a request from the shell client,
3. and flush the shell process mailbox to receive responses from the server.

```erlang
%% Compiles the server
1> c(server).
{ok,server}
%% Spawns a process and keep a reference of the server's pid.
%% Note I'm passing the shell pid `self()` as an argument to the server process.
2> ServerPid = spawn(server, server, [self()]).
<0.98.0>
%% Send messages to the server process
3> ServerPId ! {check, "madam im adam"}.
{check,"madam im adam"}
4> ServerPId ! {check, "madam im not adam"}.
{check,"madam im not adam"}
%% Then I flush the shell process mailbox
5> flush().
Shell got {result,"madam im adam is a palindrome."}
Shell got {result,"madam im not adam is not a palindrome."}
ok
```

### Handling multiple requests

The exercise goes further to handle multiple servers that respond to whatever number of requests are sent to it.

My proposed solution is to implement a function that spawns a given number of servers. Each of them will be spawned by a proxy server. A client should keep a reference to the proxy pid and send request to it. behind the scenes, the proxy delegates each incoming request to an available server. This solution also encourages a client to use an api instead of manually sending a message to a pid. Finally the we'd have to stop every server by send a `stop` message to the proxy pid, the every children server would stop.

```erlang
%% Starts 10 servers
6> ProxyPid = server:start(10).
Starting... <0.105.0>
Starting... <0.106.0>
Starting... <0.107.0>
Starting... <0.108.0>
Starting... <0.109.0>
Starting... <0.110.0>
Starting... <0.111.0>
Starting... <0.112.0>
Starting... <0.113.0>
Starting... <0.114.0>
<0.115.0>
%% Sends a check request
7> server:check(ProxyPid, "Madam Im Adam").
<0.114.0> ::: checks palindrome Madam Im Adam from: <0.83.0>
%% Sends another check request
8> server:check(ProxyPid, "Madam Im not Adam").
<0.113.0> ::: checks palindrome Madam Im not Adam from: <0.83.0>
%% Sends a stop signal to the proxy pid. Note that after sending each request to the children server, the proxy process asynchronously stops before the remaining children stop.
9> server:stop(ProxyPid).
Terminating <0.115.0>...
Terminating <0.114.0>...
stop
Terminating <0.113.0>...
Terminating <0.112.0>...
Terminating <0.111.0>...
Terminating <0.110.0>...
Terminating <0.109.0>...
Terminating <0.108.0>...
Terminating <0.107.0>...
Terminating <0.106.0>...
Terminating <0.105.0>...
```
