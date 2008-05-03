%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(id2_supervisor).
-behaviour(supervisor).

-export([start/0, start_shell/0, start_link/1, init/1]).

start() ->
  application:set_env(id2, in_rotation, true),
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).
  
start_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).
  
start_link(Args) ->
  application:set_env(id2, in_rotation, true),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  
init([]) ->
  % start the YAWS api server.
  json_server:test(),
  %erl_boot_server:add_subnet({0, 0, 0, 0}, {0, 0, 0, 0}),
  
  % return the supervisor spec for the resource fountain
  {ok, {{one_for_one, 100, 300},
    [{resource_fountain,
       {resource_fountain, start_link, []},
       permanent, 10000, worker, [resource_fountain]},
     {pool_sweeper,
       {pool_sweeper, start_link, []},
       permanent, 10000, worker, [pool_sweeper]},
     {id2_code_monitor,
       {id2_code_monitor, start_link, []},
       permanent, 10000, worker, [id2_code_monitor]}
    ]}}.
