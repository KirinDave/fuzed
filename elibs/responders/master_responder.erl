%% Yaws JSON bridge
-module(master_responder).
%-compile(export_all).
-export([out/1, yaws_handler/3, mochiweb_handler/2, rpc_translator/2, process_detail_rval/1]).
-include("yaws.hrl").
-include("yaws_api.hrl").
-include("fuzed.hrl").

-ifdef(TEST).
-include("master_responder_test.erl").
-endif.


%% START Yaws Specific Stuff
out(A) ->
  A2=A#arg{state = []},
  % yaws_jsonrpc handles the JSON-RPC packaging of the response
  % it is modified from the YAWS distro in order to bring it in
  % compliance with the true JSON-RPC spec
  yaws_jsonrpc:handler_session(A2, {?MODULE, yaws_handler}).

yaws_handler(_State, {call, Method, Value} = _Request, _Session) -> 
  Result = rpc_translator(Method,Value),
  {true, 0, hello, Result}.
%% END Yaws Specific Stuff

%% START Mochiweb Specific Stuff
mochiweb_handler(_Req, {call, Method, TupleList}) ->
  rpc_translator(Method, TupleList).
%% END Mochiweb Specific Stuff

rpc_translator(Method, TupleList) -> 
  Details = k(details, TupleList),
  Parameters = d(details, TupleList),
  ApiSpec = build_api_spec(Method,Parameters),
  case Method of
    list_available_configurations -> 
      AvailableDetails = resource_fountain:details_list(),
      {result, {array, [jsonify_full_details_list(IndividualDetail) || IndividualDetail <- AvailableDetails]}};
    configuration_for_details ->
      case resource_fountain:best_pool_for_details_match(Details) of
        none -> {error, ?NOPOOL_MSG ++ " in configuration_for_details."};
        Pool -> {result, jsonify_full_details_list(resource_pool:details(Pool))}
      end;
    exception_logging_enable ->
      response_error_logger:set_active(true),
      {result, <<"Logging Enabled.">>};
    exception_logging_disable ->
      response_error_logger:set_active(false),
      {result, <<"Logging Disabled.">>};
    current_pool_traces -> 
      {result, jsonify_full_details_list(logger:list_details_selectors())};
    start_pool_trace -> 
      logger:enable_logging(Details),
      {result, "Trace added."};
    stop_pool_trace -> 
      logger:disable_logging(Details),
      {result, "Trace removed."};
    available_methods ->
      case resource_fountain:best_pool_for_details_match(Details) of
        none -> {error, ?NOPOOL_MSG ++ " in available_methods."};
        Pool -> {result, jsonify_method_list(resource_pool:api_definition(Pool))}
      end;
    unpublish ->
      application:set_env(fuzed, in_rotation, false),
      {result, "unpublished"};
    publish ->
      application:set_env(fuzed, in_rotation, true),
      {result, "published"};
    nodes_in_pool ->
      case resource_fountain:best_pool_for_details_match(Details) of
        none -> {error, ?NOPOOL_MSG ++ " in nodes_in_pool."};
        Pool -> {result, {array, [ atom_to_list(X) || X <- uniquify_list(resource_pool:list_all_nodes(Pool))]}}
      end;
    count_workers_in_pool ->
      case resource_fountain:best_pool_for_details_match(Details) of
        none -> {result, -1};
        Pool -> {result, length(resource_pool:list(Pool))}
      end;
    _X ->
      case node_api:safely_send_call_to_pool(Method, Parameters, ApiSpec, json, Details) of
        {result, R} -> {result, R};
        {error, R}  -> response_error_logger:log_error(
                         {Details, ApiSpec}, 
                         R),
                       {error, R}
      end
  end.

   

% convenience/helper functions
k(_Key, []) -> "";
k(details, List) -> normalize_details(k_halp(details, List));
k(Key, List) when is_atom(Key) -> k_halp(Key, List);
k(Key, List) when is_list(Key) -> K = list_to_atom(Key), k_halp(K,List).


k_halp(Key, List) -> 
  case lists:keysearch(Key, 1, List)  of
    {value, {Key, Val}} -> Val ;
    Else -> Else
  end.

d([], Keylist) -> Keylist;
d([Var|Rest], Keylist) -> d(Rest, d(Var, Keylist));
d(Var, Keylist) -> lists:keydelete(Var, 1, Keylist).

normalize_details({struct, DetailTuples}) -> [normalize_detail_tuple(X) || X <- DetailTuples].

atom_to_binary(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom)).
string_to_bound_int(String) -> 
  case io_lib:fread("~d", String) of
    {ok, [Result], _} -> Result;
    {error, _}        -> 0
  end.

% @spec string_to_version_tuple(string()) -> {int(),int(),int(),int()}
string_to_version_tuple(String) -> 
  list_to_4_tup([string_to_bound_int(X) || X <- string:tokens(String, ".-")]).

% @spec process_detail_rval(string()) -> {int(),int(),int(),int()} | binary()
process_detail_rval(String) -> 
  case string:str(String,".") of
    0 -> list_to_binary(String);
    _ -> string_to_version_tuple(String)
  end.

normalize_detail_tuple({tags, Taglist}) -> {<<"tags">>, [list_to_binary(X) || X <- string:tokens(Taglist, ",")]};
normalize_detail_tuple({roles, RoleList}) -> {<<"roles">>, [list_to_binary(X) || X <- string:tokens(RoleList, ",")]};
normalize_detail_tuple({Key, Value}) when integer(Value) -> {atom_to_binary(Key), list_to_binary(io_lib:format("~p", [Value]))};
normalize_detail_tuple({Key, Value}) when is_list(Value) -> {atom_to_binary(Key), process_detail_rval(Value)};
normalize_detail_tuple({Key, Value}) when is_atom(Value) -> {atom_to_binary(Key), atom_to_binary(Value)}.

jsonify_full_details_list(DList) when is_list(DList) -> {struct, [{jsonify_details_key(X), jsonify_details_value(Y)} || {X,Y} <- DList]}.
jsonify_details_key(X) -> binary_to_list(X).
jsonify_details_value({A,B,C,D}) -> lists:flatten(io_lib:format("~p.~p.~p-~p", [A,B,C,D]));
jsonify_details_value({A,B,C}) -> lists:flatten(io_lib:format("~p.~p.~p-0", [A,B,C]));
jsonify_details_value({A,B}) -> lists:flatten(io_lib:format("~p.~p.0-0", [A,B]));
jsonify_details_value(X) when is_list(X) -> {array, [jsonify_details_value(Z) || Z <- X]};
jsonify_details_value(X) -> binary_to_list(X).

jsonify_method_list(MList) when is_list(MList) -> {struct, [{atom_to_list(X), {array, jsonify_method_args(Y)}} || {X,Y} <- MList]}.
jsonify_method_args(ATuple) when is_tuple(ATuple) -> [atom_to_list(X) || X <- tuple_to_list(ATuple)].

build_api_spec(Method,Params) ->
  {Method, list_to_tuple(lists:sort([X || {X, _} <- Params]))}.

uniquify_list(List) ->
  uniquify_list(List, []).

uniquify_list([], Accum) ->
   Accum;
uniquify_list([X|Remaining], Accum) ->
  case lists:member(X, Accum) of
	true -> uniquify_list(Remaining, Accum);
	_ -> uniquify_list(Remaining, [X|Accum])
  end.

list_to_4_tup([_A,_B,_C,_D] = L) ->
  list_to_tuple(L);
list_to_4_tup([_A,_B,_C] = L) ->
  list_to_tuple(lists:append(L, [0]));
list_to_4_tup([_A,_B] = L) ->
  list_to_tuple(lists:append(L, [0, 0]));
list_to_4_tup([_A] = L) ->
  list_to_tuple(lists:append(L, [0,0,0]));
list_to_4_tup([]) -> {0,0,0,0};
list_to_4_tup([A,B,C,D|_Rest]) -> {A, B, C, D}.
