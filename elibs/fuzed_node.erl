-module(fuzed_node).

-export([start/0]).

start() ->
  application:load(fuzed_node),
  application:start(fuzed_node).