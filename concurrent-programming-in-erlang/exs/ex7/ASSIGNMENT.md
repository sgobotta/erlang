# FutureLearn - Erlang

## 3.9

### Exercise: Upgrading the frequency server

**In this exercise we’ll run through an example of hot code upgrade, applied to the frequency server.**

Use the comments on this step to share your approaches to this exercise, any difficulties you encounter or questions you may have, and your solutions.

The original `frequency.erl` and supporting files `frequency2.erl` and `frequency3.erl` are available (as zip files) under ‘Downloads’ below.

---

#### File versions

The file f`requency2.erl` does not support live upgrade. You will first need to modify it a little in order to do this.

To perform the exercise it is suggested that you keep two versions of the `frequency` module, `frequency2.erl` and `frequency3.erl`, keeping the name of the module internally as `frequency`. When you want to use one of these alternatives as a version of `frequency.erl`, copy your alternative so that it is called `frequency.erl`, e.g. using the unix `cp` command.

---

#### Injecting new frequencies

In our frequency server example, the set of frequencies available is hard-wired in the code. It is required to add a new functionality to the server, to inject a list of frequencies into the server for future use (in addition to the ones already available).

You should build a new version of the `frequency.erl` module that:

+ can receive messages of the form `{request, Pid, {inject, Freqs}}`, where `Freqs` is a list of numbers;
+ can process this message by a function `inject/2` (defined in a similar way to the `allocate` and `deallocate` functions);
+ provides a function `inject/1` which gives a functional interface for the client to initiate an injection of frequencies.

---

#### Testing the modification

Run your frequency server from the Erlang shell and check that it has the behaviour you would expect. This would always be a preliminary to upgrading in running system: check that your upgrade behaves properly before you make it live!

---

#### Supporting live upgrade

We’ll work through the live upgrade of a running frequency server in a series of steps.

+ Run the server from the Erlang shell by calling `start/0`, and then call `allocate/1` repeatedly to allocate frequencies until there are none remaining.
+ From the shell use `c(frequency)` to compile the modified frequency module that includes `inject` functionality, and call `allocate/1` again to ensure that it is loaded.
+ Call the function `code:soft_purge(frequency)` – does it do what you would expect?
+ Call `inject/1` with a suitable argument to inject a set of new frequencies in the running server, and then call `allocate/1` to see the effect of the upgrade.

---

#### Discussion

You might like to use the discussion attached to this page to talk about other ways that hot code loading could be used in the frequency server case study, and the precautions that you would need to take to ensure that these upgrades worked without breaking the system.

---

© University of Kent
