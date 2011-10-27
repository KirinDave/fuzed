-module(yaws_master).
-export([start/3]).
-include("yaws.hrl").


start(IP, Port, DocRoot) ->
  {GC, SC} = yaws_global_configs(IP, Port, DocRoot),
  application:set_env(yaws, embedded, true),
  application:start(yaws),
  yaws_api:setconf(GC, [[SC]]).


yaws_global_configs(IP, Port, DocRoot) ->
  Y = yaws_config:yaws_dir(),
  GC = #gconf{yaws_dir = Y,
    ebin_dir = [filename:join([Y, "examples/ebin"])],
    include_dir = [filename:join([Y, "examples/include"])],
    trace = false,
    logdir = "./log",
    cache_refresh_secs = 30,
    flags =  ?GC_AUTH_LOG bor ?GC_COPY_ERRLOG bor ?GC_FAIL_ON_BIND_ERR bor ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH,
    %uid = element(2, yaws:getuid()),
    yaws = "Yaws " ++ yaws_generated:version(),
    id = genericID
  },
  SC = #sconf{port = Port,
              servername = "master_responder",
              listen = IP,
              docroot = DocRoot,
              appmods = [{"api", master_responder},
                         {"status", status_responder}]},
  {GC,SC}.

