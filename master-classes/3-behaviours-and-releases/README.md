# Master Class 3 - OTP: behaviours and releases

## Client Servers

| Generic                           | Specific                      |
| --------------------------------- | ----------------------------- |
| Spawns the **server**             | Initialises the server state  |
| Stores the loop data              | The loop data                 |
| Sends requests to the **server**  | The client requests           |
| Sends replies to the **client**   | Handles client requests       |
| Receives server **replies**       | Contents of server reply      |
| Stops the **server**              | Cleaning up                   |

### 1-Client-Server

```erlang
%% Compiles erlang modules
c(calc).
c(server).
c(expr).

%% Starts the program
Env = expr:env()
calc:start(Env).
Starting...
{ok,<0.100.0>}

%% Plays around using eval
Expr = expr:expr2()
calc:eval(Expr).
2

%% Stops the server
calc:stop().
Terminating...
ok
```

### Generic Servers

```erlang
%% Compiles erlang modules
c(calc).
c(expr).

%% Starts the program
Env = expr:env()
calc:start(Env).
Starting...
true

%% Plays around using print and eval
%% (asynchronous call)
Expr = expr:expr2()
calc:print(Expr).
((1*b)+(((2*b)+(1*b))*0))
ok
%% (synchronous call)
calc:eval(Expr).
2

%% Stops the server
%% (asynchronous call)
calc:stop().
Terminating...
stop
```

### Supervisor Behaviour

| Generic                       | Specific                                    |
| ----------------------------- | ------------------------------------------- |
| Spawning the **supervisor**   | **Supervisor** name                         |
| Starting the **children**     | What **children** to start                  |
| Monitoring the **childresn**  | Specific **child** handling `Start,Restart` |
| Restarting the **children**   | **Child** dependencies                      |
| Stopping the **supervisor**   | **Supervisor** behaviours                   |
| Cleaning up                   |                                             |

+ Supervisors are implemented in the **supervisor** module
+ The `-behaviour(supervisor).` directive must be includede in the callback module

```erlang
supervisor:start_link({local,Name},Mod,Args) -> {ok, Pid}
```

+ `supervisor:start_link/3` creates a new supervisor
  + `Name` is the process name. `Scope` can be `local` or `global`
  + `Mod` is the nake of the cllback module
  + `Args` are the arguments passed to the `init` function
+ `Mod:init/1` is called by the supervisor in the callback module
  + it returns a supervisor specification

#### `RestartType`

+ `one_for_one`:
  + only the crashed process is restarted
  + Ideal if workers don't depend on each other and the termination of one process will not affect all the others.
+ `one_for_all`:
  + all processes are terminated and restarted.
  + used if all or most of the processes in the subtree depend on each other. If a process terminates we will want to terminate them all and then restart them one by one.
+ `rest_for_one`:
  + all processes started after the crashed on are terminated and restarted.
  + we'd tend to use this strategy if you start your processes in order dependency.

#### Restart strategy

+ *Max restart*: maximum number of restarts in Max Time
+ *Max time*: if *Max Restart* is reached in *Max Time* seconds, the supervisor terminates
+ *Crashes propagate among supervisors*

#### Child specs

```erlang
ChildSpec = {Name,
              StartFunction,
              RetartType,
              ShutdownTime,
              ProcessType,
              Modules}
```

+ `Name`:
  + *unique for a particular supervisor*
  + any valid Erlang term
  + Unique for tha supervisor
+ `StartFunction`
  + `{Module, Function, Args}`
  + *must call an OTP-compliant `start_link` function (otp behaviours)*
+ `RestartType`:
  + *how to react when a child terminates*
  + `permanent`: is always restarted
  + `transient` is only restarted after a non-normal exit
  + `temporary`: is never restarted
+ `ShutdownTime`:
  + *Maximum time the process is allowed to spend in the `terminate` callback function before the supervisor inconditionally terminates the child.*
  + `Integer > 0` in ms, or `infinity` (`infinity` is used only if your child is a supervisor)
+ `ProcessType`:
  + *Used for software upgrades*
  + `supervisor` for supervisors
  + `worker` for other behaviours
+ `Modules`:
  + *List of modules implementing the child*

```erlang
%% Compiles erlang modules
c(calc).
c(expr).

%% Starts the program
Env = expr:env()
calc_sup:start_link(Env).
Starting...
{ok,<0.106.0>}

%% Plays around using print and eval
%% (asynchronous call)
Expr = expr:expr2()
calc:print(Expr).
((1*b)+(((2*b)+(1*b))*0))
ok
%% (synchronous call)
calc:eval(Expr).
2

%% Stops the server
%% (asynchronous call)
calc:stop().
Terminating...
Starting...
=SUPERVISOR REPORT==== 17-Jun-2020::02:25:15.963441 ===
    supervisor: {local,calc_sup}
    errorContext: child_terminated
    reason: normal
    offender: [{pid,<0.128.0>},
               {id,calc},
               {mfargs,{calc,start,[[{b,2}]]}},
               {restart_type,permanent},
               {shutdown,2000},
               {child_type,worker}]
%% After 10 restarts within an hour it will finally stop.
...
calc:stop().
Terminating...
ok
```

### Applications

Applications can be configured, started and stopped as a whole. Allows a system to easily manage many supervision trees, running them independently of each other.

**Normal Applications:** the ones which will start a top level supervisor. They will start the supervisor, which will start all the children, forming the supervision tree.
**Library Applications:**  contain library modules that don't start any top level supervisor. It's responsibility of other workers in other supervision trees to actually go in and start workers defined in library applications.

```erlang
%% Compiles erlang modules
c(calc).
c(expr).
c(calc_sup).
c(calc_app).

%% Starts the application
application:start(calc).
Starting...
ok

%% Plays around using print and eval
%% (asynchronous call)
Expr = expr:expr2()
catch calc:print(Expr).
((1*b)+(((2*b)+(1*b))*0))
ok
%% (synchronous call)
catch calc:eval(Expr).
-12

%% The process is running
whereis(calc).
<0.113.0>

%% Lists all tunning application
application:which_applications().
[{calc,"Calc application for the Erlang master classes",
       "1.0"},
 {stdlib,"ERTS  CXC 138 10","3.11"},
 {kernel,"ERTS  CXC 138 10","6.5.1"}]

%% Gets environment variables for the calc application
application:get_all_env(calc).
[{env,[{a,23},{b,-12}]}]

%% Stops the application
application:stop(calc).
=INFO REPORT==== 17-Jun-2020::02:42:31.002612 ===
    application: calc
    exited: stopped
    type: temporary
ok
```
