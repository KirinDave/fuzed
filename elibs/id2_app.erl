-module(id2_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  net_kernel:set_net_ticktime(30),
  id2_supervisor:start_link(StartArgs).
  
stop(_State) ->
  ok.