-module(fuzed).

-export([start/0]).

start() ->
  application:load(fuzed),
  application:start(fuzed).