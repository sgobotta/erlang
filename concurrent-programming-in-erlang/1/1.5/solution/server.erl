-module(server).
-author("Santiago Botta <santiago@camba.coop>").
-include_lib("eunit/include/eunit.hrl").
-export([server/0, server/1, proxy/1]).
-export([start_proxy/0, send_multiple_requests/3]).

start_proxy() ->
	Server1 = spawn(server, server, []),
	Server2 = spawn(server, server, []),
	Server3 = spawn(server, server, []),
	spawn(server, proxy, [[Server1, Server2, Server3]]).

send_multiple_requests(ServerPid, _From, 0) ->
	ServerPid ! stop,
	ok;
send_multiple_requests(ServerPid, From, N) ->
	ServerPid ! {check, "Madam Im Adam", From},
	send_multiple_requests(ServerPid, From, N-1).

%% Given a process id, listens to palindrome requests to return a processed
%% result.
%% Usage:
%% ServerPid = spawn(server, server, [self()]).
%% ServerPid ! {check, "MadamImAdam"}.
%% flush().
server(From) ->
	receive
		{check, String} ->
			IsPalindromeResult = is_palindrome(String),
			From ! {result, String ++ IsPalindromeResult},
			server(From);
		_Message ->
			ok
	end.


%% Takes requests from multiple clients
%% Usage:
%% ServerPid = spawn(server, server, []).
%% ServerPid ! {check, "MadamImAdam", self()}.
%% flush().
server() ->
	receive
		{check, String, From} ->
			IsPalindromeResult = is_palindrome(String),
			From ! {result, String ++ IsPalindromeResult},
			server();
		_Message ->
			ok
	end.

%% Replicating the server

%% @doc Given a list of server pids, calls a function that accepts a request to
%% one of them and distributes the next requests to the rest of the servers
%% indefinately.
%% Usage:
%% Server1 = spawn(server, server, []).
%% Server2 = spawn(server, server, []).
%% Server3 = spawn(server, server, []).
%% Proxy   = spawn(server, proxy, [[Server1, Server2, Server3]]).
proxy(Servers) ->
	proxy(Servers, Servers).

%% @doc Given a list of server pids and a pid accumulator listens to requests
%% and delegates future requests to the next pid in the servers list indefinately.
proxy([], Servers) ->
	proxy(Servers, Servers);
proxy([S|Svrs], Servers) ->
	receive
		stop ->
			stop_servers(Servers),
			ok;
		Message ->
			S ! Message,
			proxy(Svrs, Servers)
	end.

%% @doc Given a list of servers, sends a stop message to all of them.
stop_servers([]) ->
	ok;
stop_servers([S|Svrs]) ->
	S ! stop,
	stop_servers(Svrs).

%% Auxiliary functions
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
