-module(stack).

-export([start/0]).

start() ->
  fuzed:start(),
  fuzed_node:start().