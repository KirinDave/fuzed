-module(master_responder).
-export([rpc_handler/2]).
-include("../include/fuzed.hrl").

rpc_handler(_Req, {call, Method, Params}) ->
  % io:format("Request Received:~nMethod: ~p~nParams: ~p~n", [Method, Params]),
  try
    {Details, Parameters} = pop_details(Params),
    ApiSpec = build_api_spec(Method, Parameters),
    % io:format("Request Prepared:~nDetails: ~p~nApiSpec: ~p~nParameters: ~p~n",  [Details, ApiSpec, Parameters]),
    case Method of
      list_available_configurations ->
        {result, {array, [jsonify_details_list(DList) || DList <- resource_fountain:details_list()]}};
      configuration_for_details ->
        case resource_fountain:best_pool_for_details_match(Details) of
          none -> {error, ?NOPOOL_MSG ++ " in configuration_for_details"};
          Pool -> {result, jsonify_details_list(resource_pool:details(Pool))}
        end;
      available_methods ->
        case resource_fountain:best_pool_for_details_match(Details) of
          none -> {error, ?NOPOOL_MSG ++ " in available_methods."};
          Pool -> {result, jsonify_method_list(resource_pool:api_definition(Pool))}
        end;
      nodes_in_pool ->
        case resource_fountain:best_pool_for_details_match(Details) of
          none -> {error, ?NOPOOL_MSG ++ " in nodes_in_pool."};
          Pool -> {result, {array, [ atom_to_list(X) || X <- lists:usort(resource_pool:list_all_nodes(Pool))]}}
        end;
      count_workers_in_pool ->
        case resource_fountain:best_pool_for_details_match(Details) of
          none -> {result, -1};
          Pool -> {result, length(resource_pool:list(Pool))}
        end;
      _ ->
        case node_api:safely_send_call_to_pool(Method, Parameters, ApiSpec, json, Details) of
          {result, R} -> {result, R};
          {error, R}  ->
            response_error_logger:log_error({Details, ApiSpec}, R), 
            {error, R}
        end
    end
  catch
    error:bad_params_hash -> {error, "Bad params hash."};
    error:api_spec_generation -> {error, "Unable to generate API spec for request."}
  end.

pop_details({struct, ParamsList}) when is_list(ParamsList) ->
  case lists:keysearch(details, 1, ParamsList) of
    {value, {details, {struct, DetailsList}}} ->
      {normalize_details_list(DetailsList), lists:keydelete(details, 1, ParamsList)};
    _ ->
      {[], ParamsList}
  end;
pop_details(_) -> erlang:error(bad_params_hash).

build_api_spec(Method, Parameters) ->
  try
    {Method, list_to_tuple(lists:sort([X || {X,_} <- Parameters]))}
  catch
    _:_ -> erlang:error(api_spec_generation)
  end.

jsonify_method_list(MethodList) when is_list(MethodList) ->
  {struct, [{Method, {array, jsonify_method_args(Args)}} || {Method, Args} <- MethodList]}.

jsonify_method_args(Args) when is_tuple(Args) ->
  [atom_to_list(Arg) || Arg <- tuple_to_list(Args)].

jsonify_details_list(DetailsList) when is_list(DetailsList) ->
  {struct, [{jsonify_details_key(K), jsonify_details_value(V)} || {K,V} <- DetailsList]}.

jsonify_details_key(Key) -> list_to_atom(binary_to_list(Key)).

jsonify_details_value({A, B, C, D}) -> lists:flatten(io_lib:format("~p.~p.~p-~p", [A, B, C, D]));
jsonify_details_value({A, B, C}) -> lists:flatten(io_lib:format("~p.~p.~p-0", [A, B, C]));
jsonify_details_value({A, B}) -> lists:flatten(io_lib:format("~p.~p.0-0", [A, B]));
jsonify_details_value({A}) -> lists:flatten(io_lib:format("~p.0.0-0", [A]));
jsonify_details_value(Any) when is_list(Any) -> {array, [jsonify_details_value(X) || X <- Any]};
jsonify_details_value(Any) when is_binary(Any) -> binary_to_list(Any).

normalize_details_list(DetailsList) ->
  [normalize_detail_tuple(X) || X <- DetailsList].

normalize_detail_tuple({"tags", TagList}) ->
  {<<"tags">>, [list_to_binary(X) || X <- string:tokens(TagList, ",")]};
normalize_detail_tuple({"roles", RoleList}) ->
  {<<"roles">>, [list_to_binary(X) || X <- string:tokens(RoleList, ",")]};
normalize_detail_tuple({Key, Value}) when integer(Value) ->
  {atom_to_binary(Key), list_to_binary(io_lib:format("~p", [Value]))};
normalize_detail_tuple({Key, Value}) when is_list(Value) ->
  {atom_to_binary(Key), process_detail_value(Value)};
normalize_detail_tuple({Key, Value}) when is_atom(Value) ->
  {atom_to_binary(Key), list_to_binary(atom_to_list(Value))}.

atom_to_binary(Atom) when is_atom(Atom) ->
  list_to_binary(atom_to_list(Atom)).

process_detail_value(String) ->
  case string:str(String, ".") of
    0 -> list_to_binary(String);
    _ -> string_to_version_tuple(String)
  end.

string_to_version_tuple(String) ->
  list_to_4_tup([string_to_bound_int(X) || X <- string:tokens(String, ".-")]).

string_to_bound_int(String) ->
  case io_lib:fread("~d", String) of
    {ok, [Result], _} -> Result;
    {error, _} -> 0
  end.

list_to_4_tup([_A, _B, _C, _D] = L) ->
  list_to_tuple(L);
list_to_4_tup([_A,_B,_C] = L) ->
  list_to_tuple(lists:append(L, [0]));
list_to_4_tup([_A,_B] = L) ->
  list_to_tuple(lists:append(L, [0, 0]));
list_to_4_tup([_A] = L) ->
  list_to_tuple(lists:append(L, [0,0,0]));
list_to_4_tup([]) ->
  {0, 0, 0, 0};
list_to_4_tup([A,B,C,D|_Rest]) ->
  {A, B, C, D}.
