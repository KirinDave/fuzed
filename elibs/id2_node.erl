-module(id2_node).

-export([start/0]).

start() ->
  application:load(id2_node),
  application:start(id2_node).