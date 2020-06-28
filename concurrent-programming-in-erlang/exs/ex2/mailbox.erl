-module(mailbox).
-author("Santiago Botta <santiago@camba.coop>").
-include_lib("eunit/include/eunit.hrl").
-export([start/0, receiver/0]).
-export([test_bed/0]).

%% @doc A sequence of test messages. Declares a list of test messages to be sent
%%      to a mailbox process, including the `stop` message, which will
%%      eventually request the process to terminate.
%% @end
test_bed() ->
  Messages = [
    {"First message", 0},
    {"Second message", 1},
    {"Third message", 2},
    {"Fourth message", 3},
    {"Fifth message", 4},
    {"Sixth message", 5},
    {"Seventh message", 6},
    stop],
  MailboxPid = mailbox:start(),
  ok = lists:foreach(
    fun (Message) ->
      MailboxPid ! Message
    end,
    Messages
  ).

%% Returns a receiver process
start() ->
  spawn(?MODULE, receiver, []).

%% Listens for messages and prints them out.
receiver() ->
  timer:sleep(1000),
  receive
    stop ->
      io:format("Terminating the receiver process...~n"),
      ok;
    Message ->
      io:format("Message: ~p~n", [Message]),
    receiver()
  end.
