%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(fuzed_supervisor).
-behaviour(supervisor).

-export([start/0, start_shell/0, start_link/1, init/1]).

start() ->
  application:set_env(fuzed, in_rotation, true),
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
  end).

start_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).

start_link(Args) ->
  application:set_env(fuzed, in_rotation, true),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
  % start the master server
  IP = {0,0,0,0},
  Port = 9001,
  DocRoot = filename:dirname(code:which(?MODULE)) ++ "/../web",

  case application:get_env(http_server) of
    {ok, mochiweb} -> mochiweb_master:start(IP, Port, DocRoot);
    _ -> yaws_master:start(IP, Port, DocRoot)
  end,

  %erl_boot_server:add_subnet({0, 0, 0, 0}, {0, 0, 0, 0}),

  % return the supervisor spec for the resource fountain
  {ok, {{one_for_one, 100, 300},
    [{resource_fountain,
       {resource_fountain, start_link, []},
       permanent, 10000, worker, [resource_fountain]},
     {pool_sweeper,
       {pool_sweeper, start_link, []},
       permanent, 10000, worker, [pool_sweeper]},
     {fuzed_code_monitor,
       {fuzed_code_monitor, start_link, []},
       permanent, 10000, worker, [fuzed_code_monitor]},
     {logger,
       {logger, start_link, ["/var/log/fuzed"]},
       permanent, 10000, worker, [logger]}
    ]}}.
