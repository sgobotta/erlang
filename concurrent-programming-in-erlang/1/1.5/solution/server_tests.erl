-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

-spec yield() -> any().
yield() ->
  receive
    Message ->
      {ok, Message}
  after 500 ->
    timeout
end.
