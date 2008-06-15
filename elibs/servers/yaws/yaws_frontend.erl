-module(yaws_frontend).
-export([start/6, start/1]).
-include("yaws.hrl").

start(IP, Port, DocRoot, SSL, ResponderModule, AppModSpecs) ->
  {GC, SC} = yaws_global_configs(IP, Port, DocRoot, SSL, ResponderModule, AppModSpecs),
  application:set_env(yaws, embedded, true),
  application:start(yaws),
  yaws_api:setconf(GC, [[SC]]).
  
start(Conf) ->
  Env = #env{conf = {file, Conf},
             debug = false,
             trace = false,
             traceoutput = false,
             runmod = false,
             embedded = true,
             id = default},
  {ok, _GC, SC} = yaws_config:load(Env),
  application:set_env(yaws, embedded, true),
  application:start(yaws),
  GC = yaws_gc(),
  yaws_api:setconf(GC, SC).

yaws_gc() ->
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
              yaws = "Yaws " ++ yaws_generated:version(),
              id = genericID
             },
  GC.

yaws_global_configs(IP, Port, DocRoot, SSL, ResponderModule, AppModSpecs) ->
  {AppModModules, Opaques} = prepare_appmod_data(AppModSpecs),
  % io:format("DEBUG:~n~p~n---~n~p~n", [AppModModules, Opaques]),
  GC = yaws_gc(),
  SC = #sconf{port = Port,
              servername = atom_to_list(ResponderModule),
              listen = IP,
              docroot = DocRoot, 
              errormod_404 = ResponderModule,
              appmods = AppModModules,
              opaque = Opaques},
  case SSL of
    {ssl, Key, Cert} ->
      ssl:start(),
      SC2 = SC#sconf{ssl = #ssl{keyfile = Key, certfile = Cert}};
    none ->
      SC2 = SC
  end,  
  {GC,SC2}.

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
