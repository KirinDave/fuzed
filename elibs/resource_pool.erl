-module(resource_pool).
-behaviour(gen_server).

% module callbacks
-export([start/1, start_link/1, add/2, remove/2, get/1, get/2,
         refund/2, list/1, details/1, list_all/1, stop/1,
         api_signature/1, api_definition/1, is_empty/1,
		 list_nodes/1, list_all_nodes/1, is_logging/1, set_logging/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, 
  {
   active_nodes = [],
   nodes = [],
   pending_requests = queue:new(),
   node_api_signature = 0,
   node_api_definition = [],
   details = [],
   logging = false
  }
).

%% Common type annotations:
% @type details() = [{binary(), {int(),int(),int(),int()}}]
% @type resource() = pid() | {atom(), pid()}.

%
%% API
%

start_link(Details) ->
    gen_server:start_link(?MODULE, [Details], []).

start(Details) ->
  gen_server:start(?MODULE, [Details], []).

% Adds a resource to the given resource pool for vending.
% @spec add(pid(), term()) -> atom()
add(Server, Rsrc) ->
  gen_server:cast(Server, {add, Rsrc}).

% Asks a resource pool to remove a node as soon as it becomes available for removal.
% Does not fail if the node doesn't exist in this pool.
% @spec remove(pid(), resource()) -> ok
remove(Server, Rsrc) ->
  gen_server:cast(Server, {remove, Rsrc}).

% Attempts to obtain a node from the server. This may cause your process to
% block for a given timeout, possibly returning nothing
% @spec get(pid(), int()) -> resource() | atom()
get(Server, Timeout) ->
  gen_server:cast(Server, {get, self()}),
  receive 
    {resource, Rsrc} -> Rsrc
    after Timeout -> nothing
  end.

% Attempts to obtain a node from the server. This may cause your process to
% block forever.
% @spec get(pid()) -> resource()
get(Server) -> get(Server, infinity).



% Returns a node to the pool. Will not return a node to the pool 
% that has not been added.
% @spec refund(pid(), resource()) -> ok | not_a_member
refund(Server, Node) ->
  gen_server:call(Server, {refund, Node}).

% Lists all resources in the pool for a given server and currently available.
% Note: this snapshot may become invalid quickly.
% @spec list(pid()) -> [resource()]
list(Server) ->
  gen_server:call(Server, {list}).

list_nodes(Server) ->
  [node(X) || X <- list(Server)].

% List all nodes in the pool, even if they are loaned out.
% @spec list_all(pid()) -> [resource()]
list_all(Server) ->
  gen_server:call(Server, {list_all}).

list_all_nodes(Server) ->
  [node(X) || X <- list_all(Server)].

% Show the API of the pools inside. [] if pool is empty and has no details
api_definition(Server) -> 
  gen_server:call(Server, {node_api_definition}).

% Give the details associated with every node in this pool.
% FIXME
% spec details(pid()) -> details()
details(Server) -> 
  gen_server:call(Server, {details}).

% Stop the pool
stop(Server) -> 
  gen_server:cast(Server, {stop}).

is_empty(Server) -> 
  length(list_all(Server)) =:= 0.

api_signature(Server) -> gen_server:call(Server, {api_signature}).

is_logging(Server) ->
  gen_server:call(Server, {is_logging}).
  
set_logging(Server, Logging) ->
  gen_server:call(Server, {set_logging, Logging}).

%
%% gen_server callbacks
%
init([Details]) -> 
  process_flag(trap_exit, true),
  Logging = logger:is_pool_logging(self(), Details),
  {ok, #state{details=Details, logging=Logging}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({list}, _Source, State) ->
  {reply, State#state.active_nodes, State} ;
handle_call({list_all}, _Source, State) -> 
  {reply, State#state.nodes, State} ;
handle_call({pending_size}, _Source, State) -> 
  {reply, length(State#state.pending_requests), State} ;
handle_call({api_signature}, _Source, State) -> 
  {reply, State#state.node_api_signature, State};
handle_call({flush_pending}, _Source, State) -> 
  {reply, length(State#state.pending_requests), State#state{pending_requests=queue:new()}} ;
handle_call({node_api_definition}, _Source, State) -> 
  {reply, State#state.node_api_definition, State};
handle_call({details}, _Source, State) -> 
  {reply, State#state.details, State};
handle_call({is_logging}, _Source, State) ->
  {reply, State#state.logging, State};
handle_call({set_logging, Logging}, _Source, State) ->
  {reply, ok, State#state{logging=Logging}};
handle_call({refund, Resource}, _Source, State) -> 
  % Only work if this node is actually a member of the pool
  IsMember = lists:member(Resource, State#state.nodes),
  if IsMember -> 
    QueueIsEmpty = queue:is_empty(State#state.pending_requests),
    if QueueIsEmpty ->
         {reply, ok, State#state{active_nodes=lists:append(State#state.active_nodes, [Resource])}} ;
       true -> 
         {{value, Waiting}, NewQueue} = queue:out(State#state.pending_requests),
         Waiting ! {resource, Resource},
         {reply, ok, State#state{pending_requests=NewQueue}}
    end ;
    true -> 
    {reply, not_a_member, State}
  end.
    
    
  

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({stop}, State) -> 
  {stop, shutdown, State};
handle_cast({remove, Rsrc}, State) ->
  if State#state.logging -> logger:node_left(self(), Rsrc);
     true -> ok
  end,
  resource_fountain:remove_pool_if_empty(self()),
  {noreply,  State#state{ nodes=lists:delete(Rsrc, State#state.nodes),
                          active_nodes=lists:delete(Rsrc, State#state.active_nodes)}};
handle_cast({add, Rsrc}, State) -> 
  if State#state.logging -> logger:node_joined(self(), Rsrc);
     true -> ok
  end,
  NewState = insert_matching_node_once(Rsrc, State),
  link(Rsrc),
  {noreply, NewState};

handle_cast({get, For}, State) -> 
  if 
    length(State#state.active_nodes) > 0 -> 
      [Head|Rest] = State#state.active_nodes,
      For ! {resource, Head},
      {noreply, State#state{active_nodes=Rest}} ;
    true ->
      Q = queue:in(For, State#state.pending_requests),
      {noreply, State#state{pending_requests=Q}}
  end.
                          
handle_info({'EXIT', Pid, _Reason}, State) -> 
  if State#state.logging -> logger:node_left(self(), Pid);
     true -> ok
  end,
  {noreply, State#state{ nodes=lists:delete(Pid, State#state.nodes),
                         active_nodes=lists:delete(Pid, State#state.active_nodes)}};
handle_info(Any,S) -> 
  error_logger:info_msg("Got INFO ~p~n", [Any]),
  {noreply, S}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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
%% Internal API calls
%%--------------------------------------------------------------------
insert_matching_node_once(Node, #state{nodes=[]} = State) -> 
  pool_sweeper:watch(self(), Node),
  State#state{nodes=[Node], active_nodes=[Node], 
              node_api_signature=node_api:api_signature(Node),
              node_api_definition=node_api:api(Node),
              details=node_api:details(Node)};
insert_matching_node_once(Node, State) -> 
  {Nodes, ActiveNodes} = {State#state.nodes, State#state.active_nodes},
  NodeApiSignature = node_api:api_signature(Node),
  if
    NodeApiSignature =:= State#state.node_api_signature -> 
      pool_sweeper:watch(self(), Node),
      State#state{nodes=insert_node_unless_member(Node,Nodes), 
                  active_nodes=insert_node_unless_member(Node,ActiveNodes)};
    true -> 
      State
  end.

insert_node_unless_member(X, List) when is_list(List) -> 
  case lists:member(X, List) of
    true ->
      List;
    false -> 
      [X|List]
  end.
