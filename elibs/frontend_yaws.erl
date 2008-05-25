-module(frontend_yaws).
-compile(export_all).
-include("../include/yaws/yaws.hrl").


setup(Port, DocRoot, Responder) ->
  yaws_begin_server(yaws_global_configs(Port, DocRoot, Responder, [])).

setup(Port, DocRoot, Responder, AppMods) ->
  yaws_begin_server(yaws_global_configs(Port, DocRoot, Responder, AppMods)).

yaws_global_configs(Port, DocRoot, Responder, AppMods) -> 
  Y = yaws_config:yaws_dir(),
  {AppModModules, Opaques} = prepare_appmod_data(AppMods),
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
              appmods = AppModModules,
              opaque = Opaques},
  {GC,SC}.

yaws_begin_server({GC,SC}) -> 
  application:set_env(yaws, embedded, true),
  application:start(yaws),
  yaws_api:setconf(GC, [[SC]]).

% Triples: {Path, module, Role}
prepare_appmod_data(AppMods) when is_list(AppMods) ->
  lists:foldl(fun({Path, Module, Role}, {AMMM, Opaques}) -> 
                    {[{to_string(Path), Module}|AMMM], 
                     [{{to_string(Path), Module}, [{<<"roles">>, [to_binary(Role)]}]}|Opaques]}
                end,
                {[], []},
                AppMods);
prepare_appmod_data(_) -> {[], []}.

to_binary(String) when is_list(String) -> list_to_binary(String);
to_binary(Atom) when is_atom(Atom) -> to_binary(atom_to_list(Atom));
to_binary(Binary) when is_binary(Binary) -> Binary.

to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_string(String) when is_list(String) -> String.
