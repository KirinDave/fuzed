%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(id2_supervisor).
-behaviour(supervisor).

-export([start/0, start_shell/0, start_link/1, init/1]).

start() ->
  % Fix the node connections to a known range for firewalling.
  application:set_env(kernel, inet_dist_listen_min, 9200),
	application:set_env(kernel, inet_dist_listen_max, 9299),
  application:set_env(id2, in_rotation, true),
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).
  
start_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).
  
start_link(Args) ->
  application:set_env(kernel, inet_dist_listen_min, 9200),
	application:set_env(kernel, inet_dist_listen_max, 9299),
  application:set_env(id2, in_rotation, true),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  
init([]) ->
  % start the YAWS server
  json_server:test(),
  %xle_thrift_responder:start_link(9100),
  
  erl_boot_server:add_subnet({0, 0, 0, 0}, {0, 0, 0, 0}),
  
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
       permanent, 10000, worker, [id2_code_monitor]},
     {xle_thrift_responder, 
       {xle_thrift_responder, start_link, [9100]},
       permanent, 10000, worker, [xle_thrift_responder]},
     {response_error_logger,
      {response_error_logger, start_link, ["/p/log/id2/response_errors.log"]},
      permanent, 10000, worker, [response_error_logger]},
     {logger, 
       {logger, start_link, ["/p/log/id2/pools"]},
       permanent, 10000, worker, [logger]}
    ]}}.
