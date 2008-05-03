%%%-------------------------------------------------------------------
%%% File    : /Users/dfayram/Projects/concilium/elibs/resource_fountain.erl
%%% Author  : David Fayram
%%%-------------------------------------------------------------------
-module(resource_fountain).
-behaviour(gen_server).

%% API
%-export([star/0]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-ifdef(TEST).
-include("etest/resource_fountain_test.erl").
-endif.

-record(state, {details_pool_dict = dict:new(), 
                pool_details_dict = dict:new(),
                lookup_cache_dict = dict:new()}).

%% Common type annotations:
% @type details() = [{binary(), {int(),int(),int(),int()}}]
% @type call() = {atom(), {atom(), ...}}


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).


% Call to get a pool for a full details spec (often used when offering nodes to the system)
% @spec pool_for_details(details()) -> pid()
pool_for_details(Details) -> 
  gen_server:call({global, ?MODULE}, {pool_for_details, Details}).

% Lists all detail specs available in the system.
% @spec details_list() -> [details()]
details_list() -> 
  gen_server:call({global, ?MODULE}, {detail_list}).

% Given details and a call, attempts to find the right resource pool to dispatch to.
% This uses best_pool_for_details_match then does call checking. 
% @spec pool_for_dispatch(call(), details()) -> none | pid()
pool_for_dispatch(Call, PartialDetails) -> 
  case best_pool_for_details_match(PartialDetails) of
    none -> none;
    Pool -> pool_if_handles_call(Pool, Call)
  end.

% Searches for the best ResourcePool match given a partial set of details.
% @spec best_pool_for_details_match(details()) -> none | pid()
best_pool_for_details_match(PartialDetails) -> 
  gen_server:call({global, ?MODULE}, {pool_for_details_match, PartialDetails}).
  
identity() ->
  gen_server:call({global, ?MODULE}, {identity}).
  
remove_pool_if_empty(Pool) -> 
  gen_server:cast({global, ?MODULE}, {remove_pool_if_empty, Pool}).

rate_all_pools(Spec) ->
  gen_server:call({global, ?MODULE}, {rate_all_pools, Spec}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("~p starting~n", [?MODULE]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({pool_for_details, Details}, _From, State) -> 
  {Pool, NewState} = find_make_pool_for_details(Details,State#state.lookup_cache_dict,State), 
  {reply, Pool, NewState};
handle_call({detail_list}, _From, State) -> 
  {reply, dict:fetch_keys(State#state.details_pool_dict), State} ;
handle_call({pool_for_details_match, Details}, _From, State) -> 
  {{_Score, BestMatch}, NewCache} = 
    best_details_match_with_caching(Details, 
                                    State#state.details_pool_dict,
                                    State#state.lookup_cache_dict),
  if
    BestMatch == none -> 
      {reply, none, State} ;
    true -> 
      {Pool, NewState} = find_make_pool_for_details(BestMatch,NewCache,State),
      {reply, Pool, NewState}
  end;
handle_call({identity}, _From, State) ->
  {reply, {ok, node()}, State};
handle_call({rate_all_pools, Spec}, _From, State) ->
  {reply, details_match_list(Spec, State#state.details_pool_dict), State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({remove_pool_if_empty, Pool}, #state{details_pool_dict=_DetailsPoolDict,
                                                 pool_details_dict=PoolDetailsDict,
                                                 lookup_cache_dict=_OldCache} = State) when is_pid(Pool) -> 
  case lists:member(Pool, dict:fetch_keys(PoolDetailsDict)) of
    false -> {noreply, State};                  % Unrecognized pool
    true  -> 
      try
        {noreply, remove_pool_helper(resource_pool:is_empty(Pool), Pool, State)}
      catch
        Kind:Reason ->
          error_logger:error_msg("Error on pool removal: ~p:~p~n", [Kind, Reason]),
          {noreply, State}
      end
  end;
handle_cast(_Msg, State) ->
  {noreply, State}.

%% This function helps the remove_pool_if_empty cast do its job. It returns
%% a state object.
remove_pool_helper(false, _Pool, State) ->
  State;
remove_pool_helper(true, Pool, #state{details_pool_dict=DetailsPoolDict,
                                      pool_details_dict=PoolDetailsDict,
                                      lookup_cache_dict=_OldCache} = _State) ->
  case resource_pool:is_logging(Pool) of
    true -> logger:pool_removed(Pool);
    false -> ok
  end,  
  resource_pool:stop(Pool),
  PDetails = dict:fetch(Pool, PoolDetailsDict),
  #state{details_pool_dict=dict:erase(PDetails, DetailsPoolDict),
         pool_details_dict=dict:erase(Pool, PoolDetailsDict),
         lookup_cache_dict=dict:new()}.
        
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% This function creates or locates a new resourcepool() by checking
% the given dict.
% @spec find_make_pool_for_details(details(), cache(), dict()) -> pid()
find_make_pool_for_details(Details,
                           NewCache,
                           #state{details_pool_dict=DetailsPoolDict,
                                  pool_details_dict=_PoolDetailsDict,
                                  lookup_cache_dict=_OldCache} = State) -> 
  IsKey = dict:is_key(Details,DetailsPoolDict),
  if
    IsKey -> {dict:fetch(Details, DetailsPoolDict), State#state{lookup_cache_dict=NewCache}} ;
    true -> introduce_pool_for_details( Details, NewCache, State )
  end.

%% This function modifies existing state to introduce a pool which it creates.
%% It returns the new pool created along with a modified state that properly
%% incorporates it.
%% @spec introduce_pool_for_details(details(), cache(), state()) -> {pid(), state()}
introduce_pool_for_details(Details, Cache, #state{details_pool_dict=DetailsPoolDict,
                                                  pool_details_dict=PoolDetailsDict,
                                                  lookup_cache_dict=_OldCache} = _State) ->
  {ok, NewPool} = resource_pool:start_link(Details),
  case resource_pool:is_logging(NewPool) of
    true -> logger:pool_created(NewPool);
    false -> ok
  end,
  {NewPool, #state{details_pool_dict=dict:store(Details, NewPool, DetailsPoolDict),
                   pool_details_dict=dict:store(NewPool, Details, PoolDetailsDict),
                   lookup_cache_dict=Cache}}.

% This function iterates through the details given in the dict() and
% finds the details match with the highest score.
best_details_match(Ref, Dict) -> 
  Details = dict:fetch_keys(Dict),
  FoldFunc = fun(I, {ScTB, Val}) -> 
    IScore = scoring:score_details(Ref, I),
    if
      IScore > ScTB -> 
        {IScore, I};
      true -> 
        {ScTB, Val}
    end
  end,
  lists:foldl(FoldFunc, {0, none}, Details).

details_match_list(Ref, Dict) ->
  Details = dict:fetch_keys(Dict),
  MapFunc = fun(I) -> {I, scoring:score_details(Ref, I)} end,
  [X || {_Item, Score} = X <- lists:map(MapFunc, Details), Score > 0].

best_details_match_with_caching(Ref, Dict, Cache) -> 
  Key = erlang:phash2(Ref),
  case dict:find(Key, Cache) of
    {ok, Result} -> {Result, Cache};
    error        -> 
      Result = best_details_match(Ref, Dict),
      {Result, dict:store(Key, Result, Dict)}
  end.

cache_lookup_results(Search, Result, Dict) -> 
  Key = erlang:phash2(Search),
  case dict:find(Key, Dict) of
    {ok, _} -> Dict;
    error   -> dict:store(Key, Result, Dict)
  end.

pool_if_handles_call(Pool, Call) -> 
  PoolApi = resource_pool:api_definition(Pool),
  case lists:keysearch(element(1,Call), 1, PoolApi) of
    {value, Callspec} -> 
      {RParams, CParams} = {element(2,Call), element(2, Callspec)},
      if RParams =:= CParams -> Pool;
         true                -> none
      end;
    false -> none
  end.
