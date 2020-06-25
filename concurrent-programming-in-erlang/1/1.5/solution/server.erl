-module(server).
-author("Santiago Botta <santiago@camba.coop>").
-include_lib("eunit/include/eunit.hrl").
-export([server/0, server/1, proxy/1]).
-export([start/1, check/2, stop/1]).
% Testing purpose
-export([send_multiple_requests/3]).

%%%-------------------------------------------------------------------
%% @doc Server tests
%% @end
%%%-------------------------------------------------------------------

server_test() ->
  Assertions = [
    {"Adam", {result, "Adam is not a palindrome."}},
    {"Madam Im Adam", {result, "Madam Im Adam is a palindrome."}}
  ],
  Proxy = start(4),
  ok = lists:foreach(
    fun ({Request, _Response}) ->
      mock_check_request(self(), Proxy, Request)
    end,
    Assertions
  ),
  ok = lists:foreach(
    fun ({Request, Response}) ->
      ?assertEqual(Response, mock_check_response(Request))
    end,
    Assertions
  ),
  stop = server:stop(Proxy),
  ok.

mock_check_request(From, Server, Request) ->
  spawn(fun () ->
    Response = server:check(Server, Request),
    From ! {Request, Response} end
  ).

mock_check_response(Request) ->
  receive
    {Request, Response} ->
      Response
  end.

%% @doc Given a process id, listens to palindrome requests to return a processed
%%      result.
%%      Usage:
%%      ServerPid = spawn(server, server, [self()]).
%%      ServerPid ! {check, "MadamImAdam"}.
%%      flush().
%% @end
server(From) ->
  receive
    {check, String} ->
      IsPalindromeResult = is_palindrome(String),
      From ! {result, String ++ IsPalindromeResult},
      server(From);
    stop ->
      ok
  end.

%% @doc Takes requests from multiple clients
%%      Usage:
%%      ServerPid = spawn(server, server, []).
%%      ServerPid ! {check, "MadamImAdam", self()}.
%%      flush().
%% @end
server() ->
  receive
    {check, String, From} ->
      IsPalindromeResult = is_palindrome(String),
      io:format("~p ::: checks palindrome ~s from: ~p~n", [self(), String, From]),
      From ! {result, String ++ IsPalindromeResult},
      server();
    stop ->
      ok
  end.

%% Replicating the server

%% @doc Given a list of server pids, calls a function that accepts a request to
%%      one of them and distributes the next requests to the rest of the servers
%%      indefinately.
%%      Usage:
%%      Server1 = spawn(server, server, []).
%%      Server2 = spawn(server, server, []).
%%      Server3 = spawn(server, server, []).
%%      Proxy   = spawn(server, proxy, [[Server1, Server2, Server3]]).
%% @end
proxy(Servers) ->
  proxy(Servers, Servers).

%% @doc Given a list of server pids and a pid accumulator listens to requests
%%      and delegates future requests to the next pid in the servers list
%%      indefinately.
proxy([], Servers) ->
  proxy(Servers, Servers);
proxy([S|Svrs], Servers) ->
  receive
    stop ->
      lists:foreach(
        fun (Server) ->
          Server ! stop,
          io:format("Terminating ~p...~n", [Server])
        end,
        Servers
      ),
      ok;
    {check, String, From} ->
      S ! {check, String, From},
      proxy(Svrs, Servers)
  end.

%% @doc Given a list of servers, sends a stop message to each one.
stop(Server) ->
  io:format("Terminating ~p...~n", [Server]),
  Server ! stop.

%%%-------------------------------------------------------------------
%% @doc server API
%% @end
%%%-------------------------------------------------------------------

%% @doc Given an integer, spawns a proxy server with N servers as argument.
start(N) ->
  start(N, []).

%% @doc Starts N servers to return a tuple where the first component is the
%%      proxy pid and the second component the list of spawned server pids.
start(0, Servers) ->
  spawn(?MODULE, proxy, [Servers]);
start(N, Servers) ->
  Server = spawn(?MODULE, server, []),
  io:format("Starting... ~p~n", [Server]),
  start(N-1, [Server | Servers]).

%% @doc Given a server pid() and a string sends a request to the server to
%%      return an evaluated expression for a palindrome query.
-spec check(pid(), string()) -> {{atom(), string()}}.
check(Server, String) ->
  Server ! {check, String, self()},
  receive
    Response -> Response
  end.

%% @doc Given a server pid, a client pid and a number of requests, sends N
%%      similar requests to the server pid.
send_multiple_requests(_ServerPid, _From, 0) ->
  ok;
send_multiple_requests(ServerPid, From, N) ->
  From ! check(ServerPid, "Madam Im Adam"),
  send_multiple_requests(ServerPid, From, N-1).

%%%-------------------------------------------------------------------
%% @doc Palindrome Auxiliary functions
%% @end
%%%-------------------------------------------------------------------

%% @doc Given a string, returns a string telling whether it's a palindrome or not.
-spec is_palindrome(string()) -> string().
is_palindrome(String) ->
  IsPalindrome = palin:palindrome(String),
  case IsPalindrome of
    true  -> " is a palindrome.";
    false -> " is not a palindrome."
  end.

is_palindrome_test() ->
  IsPalindrome = " is a palindrome.",
  IsNotPalindrome = " is not a palindrome.",
  ?assertEqual(IsPalindrome, is_palindrome("Madam I'm Adam")),
  ?assertEqual(IsNotPalindrome, is_palindrome("Madam I'm Adams")).
