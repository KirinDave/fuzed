-module(id2).

-export([start/0]).

start() ->
  application:load(id2),
  application:start(id2).