-module(stack).

-export([start/0]).

start() ->
  id2:start(),
  id2_node:start().