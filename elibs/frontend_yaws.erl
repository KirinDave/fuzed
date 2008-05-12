-module(frontend_yaws).
-compile(export_all).
-include("../include/yaws/yaws.hrl").


setup(Port, DocRoot, Responder) ->
  yaws_begin_server(yaws_global_configs(Port, DocRoot, Responder)).

yaws_global_configs(Port, DocRoot, Responder) -> 
  Y = yaws_config:yaws_dir(),
  GC = #gconf{yaws_dir = Y,
              ebin_dir = [],
              include_dir = [],
              trace = false,
              logdir = "./log",
              cache_refresh_secs = 30,
              flags = ?GC_AUTH_LOG bor 
                      ?GC_COPY_ERRLOG bor 
                      ?GC_FAIL_ON_BIND_ERR bor 
                      ?GC_PICK_FIRST_VIRTHOST_ON_NOMATCH,
              tmpdir = yaws_config:default_tmp_dir(),
              yaws = "Yaws " ++ yaws_generated:version(),
              id = genericID
             },
  SC = #sconf{port = Port,
              servername = "xle_responder",
              listen = {0,0,0,0},
              docroot = DocRoot, 
              errormod_404 = Responder,
              appmods = []},
  {GC,SC}.

yaws_begin_server({GC,SC}) -> 
  application:set_env(yaws, embedded, true),
  application:start(yaws),
  yaws_api:setconf(GC, [[SC]]).
