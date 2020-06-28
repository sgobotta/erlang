# Concurrent Programming using Erlang

## Mailboxes: handling messages order

> Files:
>
> + [mailbox.erl](mailbox.erl)

### Introduction

This is the second exercise during the course. We were encouraged to dig into how processes in Erlang handle messages, so they introduced usto the concept of *mailboxes*. This mechanism determines how messages are handled. For example, a process would use pattern matching inside their `receive` statement to match the received messages structure. If none matches, the request is not discarded but stacked to the mailbox *tail*. Then tries with the other message received in that mailbox until one matches. If none of them triggers a coincidence to the process implementation then it just stays there until it does.

### Handle messages in order

The proposed solution was about printing messages in the order they arrive. So that we just implemented a process that sends messages to the `io` module.

```erlang
1> cd("concurrent-programming-in-erlang/exs/ex2").
ok
2> c(mailbox).
{ok,mailbox}
% Spawns and keeps a reference to a receiver process.
3> MailboxPid = mailbox:start().
<0.103.0>
% Sends a message
4> MailboxPid ! m1.
Message: m1
m1
% Sends another message
5> MailboxPid ! m2.
Message: m2
m2
```

That's fine, we're receiving the message but, how can we know they're received in order of appearance? We were encouraged to implement a test bed to send multiple message and, after a timeout, take a look at the received output.

### Messages sequence

Here's an implementation of a [test bed](mailbox.erl#11) where we declare multiple messages, spawn a process and send them one by one. We can note that after 1 second, a message is processed right after the previous one. We've also sent a `stop` message to terminate the `receiver` process.

```erlang
6> mailbox:test_bed_1().
ok
Got: "First message"
Got: "Second message"
Got: "Third message"
Got: "Fourth message"
Got: "Fifth message"
Got: "Sixth message"
Got: "Seventh message"
Terminating the receiver process...
```

> Note that even if we remove the `timeout` statement, recompile the program and run the `test_bed` again, messages are still being received in order.

### Going further with timers

What if we put a process to *sleep* for `n` seconds. Will it receive requests from a client? Here we test a few more things in the terminal. We want to put a process to sleep for a certain amount of time and send requests to it. We want to clearly notice what happens with further requests sent to the process while the process is *sleeping*.

For this test I've added a new case to the [`receiver/0`](mailbox.erl#L60) function where it accepts a `wait` message and runs a timer for 10 seconds. We'd want to start the receiver process, send some random message `m1`, then send the `wait` message and finally send a few more messages `m2`, `m3`, `m4` before the 10 seconds pass.

```erlang
7> MailboxPid2 = mailbox:start().
<0.261.0>
8> MailboxPid2 ! m1.
m1
Got: m1
9> MailboxPid2 ! wait.
Sleeping for 10 seconds. Accepting messages meanwhile...
wait
10> MailboxPid2 ! m2.
m2
11> MailboxPid2 ! m3.
m3
12> MailboxPid2 ! m4.
m4
Back from sleep state.
Got: m2
Got: m3
Got: m4
13> MailboxPid2 ! stop.
```

For this particular test I've also implemented a message [sequence test](mailbox.erl#L34), where all this messages are sent to the process without manually calling them in a shell.

```erlang
14> mailbox:test_bed_2().
ok
Got: "First message"
Got: "Second message"
Got: "Third message"
Sleeping for 10 seconds. Accepting messages meanwhile...
Back from sleep state.
Got: "Fourth message"
Got: "Fifth message"
Got: "Sixth message"
Sleeping for 10 seconds. Accepting messages meanwhile...
Back from sleep state.
Got: "Seventh message"
Terminating the receiver process...
```

### Processing messages in sequence

To define a process that will deal with messages in order I've defined a function [`process_seq/0`](mailbox.erl#L86) that listens to messages in order. This solution works for messages sent with a certain structure, it's not a general solution.

```erlang
% Starts a process_seq process
15> SeqPid = mailbox:start_seq(process_seq).
Starting sequence server...
<0.363.0>
% Sends a message that should not be processed
16> SeqPid ! {second, "Second message"}.
{second,"Second message"}
% Sends a message that should be processed first, then the last message should be processed
17> SeqPid ! {first, "First message"}.  
Processing message: "First message"
{first,"First message"}
Processing message: "Second message"
```

I've also implemented a simple [test bed](mailbox.erl#L104) to acknowledge the process responses without manually triggering messages in a shell.

```erlang
18> mailbox:process_seq_test_bed().
Starting sequence server...
Processing message: "First Message 1"
ok
Processing message: "Second Message 1"
Processing message: "First Message 2"
Processing message: "Second Message 2"
```