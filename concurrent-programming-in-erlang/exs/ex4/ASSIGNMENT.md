# FutureLearn - Erlang

## 2.12

### Exercise: Hardening the frequency server

**We have introduced process linking, exit signals, and “trapping exits”, as well as seeing how these can be applied to the frequency server example. In this exercise we’ll look in more detail at the example, and use the ‘observer’ tool to help us see what is going in when parts of a system fail.**

Use the comments here to share your approaches and solutions to this exercise. In the next step, we’ll have a discussion about how you’ve got on.

There are two supporting files, `frequency_hardened.erl` and `scenario.erl`, available (as zip files) under ‘Downloads’ below.

+ `frequency_hardened.erl` is an update of the `frequency.erl` files from previous steps - the file itself is called `frequency_hardened.erl` to distinguish it from the original, but it still defines the frequency module so you may want to re-name the file.
+ `scenario.erl` is designed to to exercise the behaviour of the hardened frequency server.

---

#### Modelling clients

In practice, a server will have multiple clients, and in order to see how the system behaves, implement a `client` function that, when spawned as a process, can be used to model a client. To allow a simulation to go on indefinitely, your function should cycle through a number of allocations and deallocations, forever (since the server is designed to live indefinitely, too).

We would like to model systems where there are multiple clients, so it would be useful to **parameterise** the client so that it can behave in different ways when called with different parameters.

---

#### The observer tool

Erlang comes with a GUI-based tool called the observer, which allows us to see, among other things, all the processes running in a system at any time, including their particular `Pid`.

To run the observer, type `observer:start()` in the erlang shell. You will see this tabbed window, which opens with an overview of the system:

<center>
  <img src="large_hero_510d69a0-1bf2-49bd-ba30-e8972970c12e.jpg">
</center>

Selecting the **Processes** tab will give a view like this:

<center>
  <img src="large_hero_ece90f09-dd71-4f4e-874c-84bbaf3741bd.jpg">
</center>

in which you can see the processes listed. Clicking the right mouse button on a process shows a menu, from which you can choose to kill the process.

---

#### Observing the frequency server and clients

Using the observer, set up a system of frequency server and at least two clients, and kill the frequency server when the clients are in possession of a frequency – you can make the clients “sleep” at any point using the `timer:sleep/1` function – observe that the clients are killed too.

---

© University of Kent
