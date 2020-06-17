-module(calc_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _Args) ->
  %% Retrieves environment variables specific to this application
  {ok,Env} = application:get_env(env),
  %% Passed to a top level supervisor, which in turn passes them to the calculator
  calc_sup:start_link(Env).

stop(_Data) ->
  ok.
