-module(fuzed_frontend).

-export([start/0]).

start() ->
  application:load(fuzed_frontend),
  application:start(fuzed_frontend).
