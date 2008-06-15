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
  case Master of
    {ok, MasterNode} ->
      ping_master(MasterNode);
    undefined ->
      MasterNode = node()
  end,
  
  case application:get_env(pidfile) of
    {ok, Location} ->
      Pid = os:getpid(),
      ok = file:write_file(Location, list_to_binary(Pid));
    undefined -> ok
  end,
  
  case application:get_env(conf) of
    {ok, Conf} ->
      yaws_frontend:start(Conf);
    undefined ->
      IP = {0,0,0,0},
      {ok, Port} = application:get_env(port),
      {ok, DocRoot} = application:get_env(docroot),
      SSL = ssl_config(),
      ResponderModule = figure_responder(),
      AppModSpecs = process_appmods(application:get_env(appmods)),
  
      case application:get_env(http_server) of
        {ok, mochiweb} -> mochiweb_frontend:start(IP, Port, DocRoot, SSL, ResponderModule, AppModSpecs);
        _ -> yaws_frontend:start(IP, Port, DocRoot, SSL, ResponderModule, AppModSpecs)
      end
  end,
  
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