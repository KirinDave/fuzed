-module(api_responder).
-export([out/1, rpc_response_point/2, rpc_response_point/3]).
-export([mochiweb_handler/2, mochiweb_rpc_response/2]).
-include("yaws_api.hrl").

mochiweb_handler(Req, Details) ->
  put(details, Details),
  mochiweb_rpc:handler(Req, {?MODULE, mochiweb_rpc_response}).

mochiweb_rpc_response(_Req, {call, Method, {struct, TupleList}}) ->
  rpc_response_point(Method, TupleList).

out(Arg) ->
  put(details, details_for_arg(Arg)), % Into the process dictionary we go. State rules!
                                      % Down with no state!
  yaws_jsonrpc:handler_session(Arg, {?MODULE, rpc_response_point}).

details_for_arg(Arg) ->
  Opaque = Arg#arg.opaque,
  Key = {lists:filter(fun(Char) -> Char =/= $/ end, Arg#arg.server_path),
         ?MODULE},
  case lists:keysearch(Key, 1, Opaque) of
    false ->
      [] ;
    {value, {_, V}} ->
      V
  end.

rpc_response_point(_State, {call, Method, Value} = _Request, _Session) -> 
  Result = rpc_response_point(Method,Value),
  {true, 0, hello, Result}.
rpc_response_point(Method, TupleList) ->
  Details = get(details),
  Parameters = TupleList,
  ApiSpec = build_api_spec(Method,Parameters),
  io:format( "Request prepped:~nDetails: ~p~nApiSpec: ~p~nParameters: ~p~n", 
             [Details, ApiSpec, Parameters] ),
  case node_api:safely_send_call_to_pool(Method, Parameters, ApiSpec, json, Details) of
    {result, R} -> {result, R};
    {error, R}  -> response_error_logger:log_error(
                     {Details, ApiSpec}, 
                     R),
                   {error, R}
  end.
    
build_api_spec(Method,Params) ->
  {Method, list_to_tuple(lists:sort([X || {X, _} <- Params]))}.
