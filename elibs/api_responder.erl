-module(api_responder).
-export([out/1, rpc_response_point/2]).
-include("yaws_api.hrl").

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

rpc_response_point(Method, TupleList) ->
  {_, Details} = get(details),
  Parameters = TupleList,
  ApiSpec = build_api_spec(Method,Parameters),
  case node_api:safely_send_call_to_pool(Method, Parameters, ApiSpec, json, Details) of
    {result, R} -> {result, R};
    {error, R}  -> response_error_logger:log_error(
                     {Details, ApiSpec}, 
                     R),
                   {error, R}
  end.
    
build_api_spec(Method,Params) ->
  {Method, list_to_tuple(lists:sort([X || {X, _} <- Params]))}.
