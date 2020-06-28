# Erlang Master Classes

This master clases consist in three different classes, each followed by a serie of videos:

## Erlang Master Class 1

+ **Language Processing**
+ *Simon Thompson*

There we'll have an insight to structured data, parsing, printing and simplification data processing.

[Youtube Playlist](https://www.youtube.com/playlist?list=PLR812eVbehlwEArT3Bv3UfcM9wR3AEZb5)

## Erlang Master Class 2

+ **Concurrent Programming**
+ *Joe Armstrong*

During this class we'll explore the OTP (*Open Telecom Platform*) processes. We'll be using the `expr` module writen by Simon Thompson in the first lecture to mimic concurrency in Erlang.

+ Concurrency Primitives: `spawn`, `send`, `receive`, `self`
+ Timeouts
+ Processes primitives: `register`, `whereis`
+ Trapping errors
+ How to build our own concurrency abstractions
+ Turn sequential code into concurrent code
+ Error handling
+ First look at generics
+ Start on a name-server resolver - client - server framework

> [Dns Evaluator](2-concurrent-programming/dns/README)

[Youtube Playlist](https://www.youtube.com/playlist?list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn)

## Erlang Master Class 3

+ **OTP: behaviours and releases**
+ Francesco Cesarini

There we'll dig deep into generic servers as processes, optional parameters, building and releasing a calculator application.

+ The road to generics using client-servers
+ Writing your own generic server module
+ Fault tolerance and supervision trees
+ Understand the importance of abstracting message passing
+ Look at race conditions in concurrent systems
+ A safe approach to sending and receiving requests
+ Generic server callback functions
+ Starting and stopping generic servers
+ Synchronous and asynchronous message passing
+ Generic supervisor module
+ Starting static behaviour processes
+ Restart strategies
+ Packaging OTP Supervision Trees in Applications
+ Application directory structures
+ Application Resource Files
+ Starting and Stopping Applications
+ Learn the importance of separating generic and specific code
+ OTP behaviours
+ Encapsulating supervision trees in an application and building a release
+ Learn how to package them into applications, used to create releases

[Youtube Playlist](https://www.youtube.com/playlist?list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc)
