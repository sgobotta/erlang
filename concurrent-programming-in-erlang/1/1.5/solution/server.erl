-module(server).
-author("Santiago Botta <santiago@camba.coop>").
-include_lib("eunit/include/eunit.hrl").
-export([server/0, server/1]).

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
		_Message -> ok
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
		_Message -> ok
	end.

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
