-module(node_api).
-author("Dave Fayram").
% Functional calls
-export([parse_sentence/2, semrep_for_sentence/2, nlmatch_semreps/3, details/1, stop/1, heat/1, morphemes_for_sentence/2,
         api/1, api_signature/1, safely_send_call_to_pool/4, safely_send_call_to_pool/5, send_call/3, send_call/4,
         heat/2, safely_send_call_to_pool_no_lookup/4]).

-include("fuzed.hrl").

-ifdef(TEST).
-include("../etest/node_api_test.erl").
-endif.

independent_pool_call(Pool, Method, Parameters, RetType, Parent) ->
  Resource = resource_pool:get(Pool),
  Result = 
  try 
    case is_remote_process_alive(Resource) of
    true -> send_call(Resource, Method, RetType, Parameters);
      false -> parent_died
    end
  after 
    resource_pool:refund(Pool, Resource)
  end,
  Parent ! {self(), result, Result},
  done.

safely_send_call_to_pool(Method, Parameters, ApiSpec, Details) -> 
  safely_send_call_to_pool(Method, Parameters, ApiSpec, pure, Details).
% Valid retkinds are json and erlang  
safely_send_call_to_pool(Method, Parameters, ApiSpec, RetType, Details) -> 
  PoolSearch = resource_fountain:pool_for_dispatch(ApiSpec, Details),
  safely_send_call_to_pool_no_lookup(Method, Parameters, RetType, PoolSearch).

safely_send_call_to_pool_no_lookup(Method, Parameters, RetType, Pool) ->
  Me = self(),
  case Pool of
    none -> {error, ?NOPOOL_MSG ++ " in safely_send_call_to_pool_with_no_lookup."};
    Pool when is_pid(Pool) ->
      Worker = spawn(fun() -> independent_pool_call(Pool, Method, Parameters, RetType, Me) end),
      receive
        {Worker, result, Result} -> Result
      after ?GLOBAL_TIMEOUT -> {error, io_lib:format("Request took more than 1 minute. Method: ~p~nParameters: ~p~n", [Method, Parameters])}
      end
  end.

send_call(Port, Method, Parameters) ->
  send_call(Port, Method, pure, Parameters).

send_call(Port, Method, RetType, Parameters) -> 
  Call = [Method,RetType|prepare_parameters(Parameters)], 
  case send_call_object_to_port(Port, Call) of
    {error, Error} ->
      {error, binary_to_list(Error)};
    {Port, timed_out} -> 
      {error, "Node timeout!"};
    % {result, Result} is expected, or {result {raw, Result}}
    X -> 
      X
  end.

send_call_object_to_port(Port, Call) -> 
  port_wrapper:send(Port, {call, Call}),
  receive
    {Port, Value} -> Value
  end.

parse_sentence(Port, Sentence) -> 
  BSent = list_to_binary(Sentence),
  port_wrapper:send(Port, {parse, BSent}),
  receive
    {Port, {result, Fstruct}} -> Fstruct
  end.
  
morphemes_for_sentence(Port, Sentence) -> 
  BSent = list_to_binary(Sentence),
  port_wrapper:send(Port, {morphemes_for_sentence, BSent}),
  receive
    {Port, {result, Morphs}} -> Morphs
  end.
  
semrep_for_sentence(Port, Sentence) -> 
  BSent = list_to_binary(Sentence),
  port_wrapper:send(Port, {parse, BSent}),
  receive
    {Port, {result, Semrep}} -> Semrep
  end.

nlmatch_semreps(Port, Semrep1, Semrep2) -> 
  port_wrapper:send(Port, {nlmatch, Semrep1, Semrep2}),
  receive
    {Port, {result, Matchstring}} -> Matchstring
  end.
  
details(Port) -> 
  port_wrapper:send(Port, config),
  receive 
    {Port, {result, Details}} -> process_details(Details)
  end.

heat(Port) -> 
  Port ! {self(), heat},
  receive
    {Port, hot} -> ok
  end.

heat(Port, Timeout) -> 
  Port ! {self(), heat},
  receive
    {Port, hot} -> ok
    after Timeout -> fail
  end.

api(Port) -> 
  Port ! {self(), api},
  receive
    {Port, API} -> API
  after ?SLEEP_CYCLE -> 
    0
  end.

api_signature(Port) -> erlang:phash2(api(Port)).

stop(Port) -> 
  Port ! shutdown,
  ok.

process_details(DTuple) -> 
  Details = erlang:tuple_to_list(DTuple),
  Listifiers = [<<"tags">>, <<"roles">>],
  F = fun(Key, Intails) -> listify_detail_for_key(Key, Intails) end,
  lists:map(fun({K,V}) -> {K, purify_detail_value(V)} end, lists:foldl(F, Details, Listifiers)).

listify_detail_for_key(Key, Details) -> 
  case lists:keysearch(Key, 1, Details) of
    {value, KeyTuple} -> 
      [Key|Rest] = erlang:tuple_to_list(KeyTuple),
      lists:keyreplace(Key, 1, Details, {Key, Rest}) ;
    false -> 
      [{Key, []}|Details]
  end.

prepare_parameters(L) when is_list(L) -> 
  [{K,prepare_pvalue(V)} || {K,V} <- L].

atom_to_binary(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom)).
prepare_pvalue({struct, L}) when is_list(L) -> {struct, [prepare_pvalue(X) || X <- L]};
prepare_pvalue({array, L}) when is_list(L) -> {array, [prepare_pvalue(X) || X <- L]};
prepare_pvalue({Atom, V}) when is_atom(Atom) -> {atom_to_binary(Atom), prepare_pvalue(V)};
prepare_pvalue(L) when is_list(L) ->  list_to_binary(xmerl_ucs:to_utf8(L));
prepare_pvalue(V) -> V.

is_remote_process_alive(Pid) ->
  Node = node(Pid),
  case rpc:call(Node, erlang, is_process_alive, [Pid]) of
    {badrpc, _Reason} -> false;
    Result -> Result
  end.

purify_detail_value({A,B,C}) -> {A, B, C, 0};
purify_detail_value({A,B}) -> {A, B, 0, 0};
purify_detail_value({A}) -> {A, 0, 0, 0};
purify_detail_value(X) -> X.
