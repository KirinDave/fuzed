-module(fuzed_frontend_supervisor).
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
  {ok, DocRoot} = application:get_env(docroot),
  {ok, Port} = application:get_env(port),
  AppModSpecs = process_appmods(application:get_env(appmods)),
  ResponderModule = figure_responder(),
  case Master of
    {ok, MasterNode} ->
      ping_master(MasterNode);
    undefined ->
      MasterNode = node()
  end,
  SSL = ssl_config(),
  frontend_yaws:setup(Port, DocRoot, ResponderModule, AppModSpecs, SSL),
  {ok, {{one_for_one, 10, 600},
        [{master_beater,
          {master_beater, start_link, [MasterNode, ?GLOBAL_TIMEOUT, ?SLEEP_CYCLE]},
          permanent,
          10000,
          worker,
          [master_beater]}
        ]}}.
  

ssl_config() ->
  case application:get_env(ssl_key) of
    {ok, Key} -> ok;
    undefined -> Key = undefined
  end,
  case application:get_env(ssl_cert) of
    {ok, Cert} -> ok;
    undefined -> Cert = undefined
  end,
  case {Key, Cert} of
    {undefined, _Any1} -> none;
    {_Any2, undefined} -> none;
    _Else -> {ssl, Key, Cert}
  end.
  
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

figure_responder() ->
  case application:get_env(responder) of
    {ok, Module} ->
      Module;
    undefined -> frontend_responder
  end.
      
process_appmods(undefined) -> [];
process_appmods({ok, V}) -> V.
