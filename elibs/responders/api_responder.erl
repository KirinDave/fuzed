-module(api_responder).
-export([out/1, yaws_handler/3]).
-export([mochiweb_handler/3, mochiweb_rpc_handler/2]).
-export([rpc_translator/2]).
-include("yaws_api.hrl").

%% START Yaws Specific Stuff
out(Arg) ->
  put(details, yaws_extract_details(Arg)),  % Into the process dictionary we go. State rules!
                                            % Down with no state!
  yaws_jsonrpc:handler_session(Arg, {?MODULE, yaws_handler}).

yaws_extract_details(Arg) ->
  Opaque = Arg#arg.opaque,
  Key = {lists:filter(fun(Char) -> Char =/= $/ end, Arg#arg.server_path), ?MODULE},
  case lists:keysearch(Key, 1, Opaque) of
    false -> [] ;
    {value, {_, V}} -> V
  end.

yaws_handler(_State, {call, Method, Value} = _Request, _Session) ->
  Result = rpc_translator(Method,Value),
  {true, 0, hello, Result}.
%% END Yaws Specific Stuff

%% START Mochiweb Specific Stuff
mochiweb_handler(Req, _DocRoot, Details) ->
  put(details, Details),
  mochiweb_rpc:handler(Req, {?MODULE, mochiweb_rpc_handler}).

mochiweb_rpc_handler(_Req, {call, Method, {struct, TupleList}}) ->
  rpc_translator(Method, TupleList).
%% END Mochiweb Specific Stuff

rpc_translator(Method, TupleList) ->
  Details = get(details),
  Parameters = TupleList,
  ApiSpec = build_api_spec(Method,Parameters),
  % io:format( "Request prepped:~nDetails: ~p~nApiSpec: ~p~nParameters: ~p~n",  [Details, ApiSpec, Parameters] ),
  case node_api:safely_send_call_to_pool(Method, Parameters, ApiSpec, json, Details) of
    {result, Result} -> {result, Result};
    {error, Result}  ->
      response_error_logger:log_error({Details, ApiSpec}, Result),
      {error, Result}
  end.

build_api_spec(Method,Params) ->
  {Method, list_to_tuple(lists:sort([X || {X, _} <- Params]))}.
