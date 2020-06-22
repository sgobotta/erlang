-module(server).
-author("Santiago Botta <santiago@camba.coop>").
-export([server/1]).

server(Pid) ->
	receive
		{check, String} ->
			case palin:palindrome(String) of
				true  -> Pid ! {result, String ++ " is a palindrome."};
				false -> Pid ! {result, String ++ " is not a palindrome."},
				server(Pid)
			end;
		_Message -> ok
	end.
