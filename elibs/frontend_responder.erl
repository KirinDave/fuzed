-module(frontend_responder).
-include("responder_base.erl").
-compile(export_all).

% This responder can modify the request, and is obligated to return
% a pool and the request. You can just return it directly if you want.

provide_pool(A, _GC, _SC) ->
  {resource_fountain:best_pool_for_details_match(details()),
   A}.

% TODO: Not ready for railsconf, I'll fix if I have time. If you want to contribute to
% fuzed, anonymous contributor, here is a great place to start.

%% provide_pool(A, _GC, _SC) ->
%%   % I confess this approach is lazy, but I don't think it's slow.
%%   case application:get_env(fuzed_frontend, current_pool) of
%%     undefined -> 
%%       Pool = lookup_and_cache_pool(),
%%       {Pool, A};
%%     {ok, Pool} ->
%%       case Pool of
%%         none ->
%%           {none, A};
%%         _ when is_pid(Pool) ->
%%                  case is_remote_process_alive(Pool) of
%%                    true ->
%%                      {Pool, A};
%%                    false -> 
%%                      {lookup_and_cache_pool(), A}
%%                  end
%%              end
%%   end.

%% lookup_and_cache_pool() ->
%%   Pool = resource_fountain:best_pool_for_details_match(details()),
%%   case Pool of
%%     none -> 
%%       application:set_env(fuzed_frontend, current_pool, undefined),
%%       none;
%%     X ->
%%       application:set_env(fuzed_frontend, current_pool, X),
%%       X
%%   end.

%% is_remote_process_alive(Pid) ->
%%   Node = node(Pid),
%%   case rpc:call(Node, erlang, is_process_alive, [Pid]) of
%%     {badrpc, _Reason} -> false;
%%     Result -> Result
%%   end.
