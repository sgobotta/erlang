-module(server).
-author("Santiago Botta <santiago@camba.coop>").
-export([server/0]).

server() ->
	receive
		{check, String, Pid} ->
			case palin:palindrome(String) of
				true  -> Pid ! {result, String ++ " is a palindrome."};
				false -> Pid ! {result, String ++ " is not a palindrome."}
			end,
			server();
		_Message -> ok
	end.
