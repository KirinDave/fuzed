%% @author Abhay Kumar <abhay@opensynapse.net>

-module(frontend_server).
-author("Abhay Kumar <abhay@opensynapse.net>").
-export([start/5]).
-export([request_loop/4]).

start(IP, Port, DocRoot, ResponderModule, AppModSpecs) ->
  ReqLoopFun = fun(Req) -> ?MODULE:request_loop(Req, DocRoot, ResponderModule, AppModSpecs) end,
  mochiweb_http:start([{name, ?MODULE}, {ip, IP}, {port, Port}, {loop, ReqLoopFun}]).

request_loop(Req, DocRoot, ResponderModule, AppModSpecs) ->
  "/" ++ Path = Req:get(path),
  case modded_path(Path, AppModSpecs) of
    {NewResponderModule, Details} ->
      NewResponderModule:mochiweb_handler(Req, Details);
    none ->
      ResponderModule:mochiweb_handler(Req, DocRoot, [])
  end.

modded_path(_Path, []) -> none;
modded_path(_Path, [{_Path, Module, Role}|_Rest]) -> {Module, [{<<"roles">>, [list_to_binary(atom_to_list(Role))]}]};
modded_path(Path, [Tuple|Rest]) when is_tuple(Tuple) -> modded_path(Path, Rest).
