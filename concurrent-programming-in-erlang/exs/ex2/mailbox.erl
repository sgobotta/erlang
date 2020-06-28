-module(mailbox).
-author("Santiago Botta <santiago@camba.coop>").
-export([start/0, receiver/0]).
-export([test_bed_1/0, test_bed_2/0]).
-export([start_seq/1, process_seq/0, process_seq_test_bed/0]).

%% @doc A sequence of test messages. Declares a list of test messages to be sent
%%      to a mailbox process, including the `stop` message, which will
%%      eventually request the process to terminate.
%% @end
test_bed_1() ->
  Messages = [
    "First message",
    "Second message",
    "Third message",
    "Fourth message",
    "Fifth message",
    "Sixth message",
    "Seventh message",
    stop],
  MailboxPid = mailbox:start(),
  ok = lists:foreach(
    fun (Message) ->
      MailboxPid ! Message
    end,
    Messages
  ).

%% @doc A sequence of test messages. Declares a list of test messages to be sent
%%      to a mailbox process, including the `wait` message, which will turn the
%%      process into a sleep state. Meanwhile other messages are sent before
%%      the `stop` message is received.
%% @end
test_bed_2() ->
  Messages = [
    "First message",
    "Second message",
    "Third message",
    wait,
    "Fourth message",
    "Fifth message",
    "Sixth message",
    wait,
    "Seventh message",
    stop],
  MailboxPid = mailbox:start(),
  ok = lists:foreach(
    fun (Message) ->
      MailboxPid ! Message
    end,
    Messages
  ).

%% Returns a receiver process
-spec start() -> pid().
start() ->
  spawn(?MODULE, receiver, []).

%% Listens for messages and prints them out.
-spec receiver() -> any().
receiver() ->
  receive
    stop ->
      io:format("Terminating the receiver process...~n"),
      ok;
    wait ->
      io:format("Sleeping for 10 seconds. Accepting messages meanwhile...~n"),
      timer:sleep(10000),
      io:format("Back from sleep state.~n"),
      receiver();
    Message ->
      timer:sleep(1000),
      io:format("Got: ~p~n", [Message]),
      receiver()
  end.

%% @doc Returns a `process_seq` pid.
%% @end
-spec start_seq(atom()) -> pid().
start_seq(Server) ->
  io:format("Starting sequence server...~n"),
  spawn(?MODULE, Server, []).

%% @doc Listens to messages in sequence.
%% @end
-spec process_seq() -> any().
process_seq() ->
  receive
    {first, FirstString} ->
      process_message(FirstString)
  end,
  receive
    {second, SecondString} ->
      process_message(SecondString)
  end,
  process_seq().

%% @doc Given a string, prints out its value with a efedback message.
%% @end
-spec process_message(string()) -> ok.
process_message(Message) ->
  io:format("Processing message: ~p~n", [Message]).

-spec process_seq_test_bed() -> ok.
process_seq_test_bed() ->
  Messages = [
    {second, "Second Message 1"},
    {second, "Second Message 2"},
    {first, "First Message 1"},
    {first, "First Message 2"}],
  SeqPid = mailbox:start_seq(process_seq),
  ok = lists:foreach(
    fun (Message) ->
      SeqPid ! Message
    end,
    Messages
  ).
