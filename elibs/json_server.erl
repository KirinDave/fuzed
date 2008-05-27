-module(json_server).
-compile(export_all).
-include("../include/yaws/yaws.hrl").


test() -> 
  ModuleFilePath = code:which(?MODULE),
  DirPath = filename:dirname(ModuleFilePath),
  yaws_begin_server(yaws_global_configs(9001, DirPath ++ "/../web")).


yaws_global_configs(Port, DocRoot) -> 
  Y = yaws_config:yaws_dir(),
  GC = #gconf{yaws_dir = Y,
    ebin_dir = [filename:join([Y, "examples/ebin"])],
    include_dir = [filename:join([Y, "examples/include"])],
    trace = false,
    logdir = "./log",
    cache_refresh_secs = 30,
    flags =  ?GC_AUTH_LOG bor ?GC_COPY_ERRLOG bor ?GC_FAIL_ON_BIND_ERR bor ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH,
    %uid = element(2, yaws:getuid()),
    tmpdir = yaws_config:default_tmp_dir(),
    yaws = "Yaws " ++ yaws_generated:version(),
    id = genericID
  },
  SC = #sconf{port = Port,
              servername = "json_responder",
              listen = {0,0,0,0},
              docroot = DocRoot, 
              appmods = [{"api", generic_json_responder},
                         {"status", fuzed_status}]},
  {GC,SC}.

yaws_begin_server({GC,SC}) -> 
  application:set_env(yaws, embedded, true),
  application:start(yaws),
  yaws_api:setconf(GC, [[SC]]).
