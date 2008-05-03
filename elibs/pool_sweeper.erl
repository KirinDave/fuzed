%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(pool_sweeper).
-behaviour(gen_server).

%% API
-export([start_link/0, watch/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(state, 
  {
    node_pids = dict:new(),
    pid_pools = dict:new()
  }
).

%%====================================================================
%% API
%%====================================================================

watch(Pool, Pid) -> 
  gen_server:cast(?MODULE, {watch, Pool, Pid}).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
  error_logger:info_msg("~p starting~n", [?MODULE]),
  net_kernel:monitor_nodes(true, [nodedown_reason]),
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
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({watch, Pool, Pid}, State) ->
  Node = node(Pid),
  error_logger:info_msg("Now monitoring node ~p~n", [Node]),
  #state{node_pids=NodePids, pid_pools=PidPools} = State,
  
  PidPoolsNew = dict:store(Pid, Pool, PidPools),
  
  case dict:find(Node, NodePids) of
    {ok, _Pids} ->
      NodePidsNew = dict:append(Node, Pid, NodePids);
    error ->
      NodePidsNew = dict:store(Node, [Pid], NodePids)
  end,
  
  {noreply, State#state{node_pids=NodePidsNew, pid_pools=PidPoolsNew}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({nodeup, Node, _}, State) ->
  error_logger:info_msg("Node ~p joined.~n", [Node]),
  {noreply, State};
handle_info({nodedown, Node, Reason}, State) ->
  error_logger:warning_msg("Node ~p went away because ~p. Removing from pools.~n", [Node, Reason]),
  #state{node_pids=NodePids, pid_pools=PidPools} = State,
  case dict:find(Node, NodePids) of
    error -> 
      {noreply, State};
    {ok, Pids} -> 
      PidPoolsNew = remove_pids_from_pools(Pids, PidPools),
      NodePidsNew = dict:erase(Node, NodePids),
      sweep_pools_from_fountain(Pids, PidPools),
      {noreply, State#state{node_pids=NodePidsNew, pid_pools=PidPoolsNew}}
  end.

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

remove_pids_from_pools([], PidPools) ->
  PidPools;
remove_pids_from_pools([Pid|Rest], PidPools) ->
  case dict:find(Pid, PidPools) of
    error -> remove_pids_from_pools(Rest, PidPools);
    {ok, Pool} -> 
      resource_pool:remove(Pool, Pid),
      PidPoolsNew = dict:erase(Pid, PidPools),
      remove_pids_from_pools(Rest, PidPoolsNew)
  end.

sweep_pools_from_fountain(Pids, PidPools) -> spff_helper([dict:fetch(X, PidPools) || X <- Pids], []).
spff_helper([], _) -> ok;
spff_helper([Pool|Rest], AlreadyDone) -> 
  case lists:member(Pool, AlreadyDone) of
    false -> resource_fountain:remove_pool_if_empty(Pool), spff_helper(Rest, [Pool|AlreadyDone]);
    true  -> spff_helper(Rest,AlreadyDone)
  end.
