%% @author Abhay Kumar <abhay@opensynapse.net>

-module(mochiweb_master).
-author("Abhay Kumar <abhay@opensynapse.net>").
-export([start/3, stop/0]).
-export([request_loop/2]).

start(IP, Port, DocRoot) ->
  ReqLoopFun = fun(Req) -> ?MODULE:request_loop(Req, DocRoot) end,
  mochiweb_http:start([{name, ?MODULE}, {ip, IP}, {port, Port}, {loop, ReqLoopFun}]).

stop() ->
  mochiweb_http:stop(?MODULE).

request_loop(Req, DocRoot) ->
  "/" ++ Path = Req:get(path),
  FirstSlash = hd(string:tokens(Path, "/")),
  case {Req:get(method), FirstSlash} of
    {'POST', "api"} ->
      mochiweb_rpc:handler(Req, {master_responder, rpc_handler});
    {'POST', _} ->
      Req:not_found();
    {Method, "status"} when Method =:= 'GET'; Method =:= 'HEAD' ->
      status_responder:mochiweb_handler(Req);
    {Method, _} when Method =:= 'GET'; Method =:= 'HEAD' ->
      Req:serve_file(Path, DocRoot);
    {_, _} ->
      Req:respond({501, [], []})
  end.