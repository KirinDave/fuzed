%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(fuzed_node_supervisor).
-behaviour(supervisor).
-export([start/0, start_shell/0, start_link/1, init/1]).
-include("../include/fuzed.hrl").

% Supervisor Functions

start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).
  
start_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).
  
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  

init([]) ->
  Master = application:get_env(master),
  case Master of
    {ok, MasterNode} ->
      ping_master(MasterNode);
    undefined ->
      MasterNode = node()
  end,
  {ok, NumNodes} = application:get_env(num_nodes),
  {ok, Spec} = application:get_env(spec),
  init_helper(MasterNode, NumNodes, Spec).
  

% Helper functions

ping_master(Node) -> 
  case net_adm:ping(Node) of
    pong -> 
      timer:sleep(?SLEEP_CYCLE),
      ok;
    pang -> 
      error_logger:info_msg("Master node ~p not available. Retrying in 5 seconds.~n", [Node]),
      timer:sleep(?SLEEP_CYCLE),
      ping_master(Node)
  end.

init_helper(Master, NumNodes, Spec) when integer(NumNodes) ->
  error_logger:info_msg("Starting with ~p set(s) of nodes.~n", [NumNodes]), 
  start_rm(Master, explode_spec(Spec, NumNodes)).
  
explode_spec(Specs, N) -> 
  SBins = [list_to_binary(X) || X <- Specs],
  lists:sort([binary_to_list(X) || X <- lists:flatten(lists:duplicate(N, SBins))]).

start_rm(Master, NodeSpec) ->
  error_logger:info_msg("Starting with spec: ~p~n", [NodeSpec]),
  Maker = fun(N) -> 
    node_api:heat(N, ?HEAT_DURATION)
  end,
  Killer = fun(X) -> X end, 
  {ok, {{one_for_one, 10, 600},
    [{resource_manager,
      {resource_manager, start_link, [Master, NodeSpec, Maker, Killer, ?GLOBAL_TIMEOUT]},
      permanent,
      10000,
      worker,
      [resource_manager]},
     {master_beater,
      {master_beater, start_link, [Master, ?GLOBAL_TIMEOUT, ?SLEEP_CYCLE]},
      permanent,
      10000,
      worker,
      [master_beater]}
    ]}}.
