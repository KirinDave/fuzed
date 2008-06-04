%% @author Abhay Kumar <abhay@opensynapse.net>

-module(frontend_server).
-author("Abhay Kumar <abhay@opensynapse.net>").
-export([start/5]).
-export([request_loop/4]).

start(IP, Port, DocRoot, ResponderModule, AppModSpecs) ->
  ReqLoopFun = fun(Req) -> ?MODULE:request_loop(Req, DocRoot, ResponderModule, AppModSpecs) end,
  mochiweb_http:start([{name, ?MODULE}, {ip, IP}, {port, Port}, {loop, ReqLoopFun}]).

request_loop(Req, DocRoot, ResponderModule, _AppModSpecs) ->
  ResponderModule:handler(Req, DocRoot).