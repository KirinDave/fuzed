%%%-------------------------------------------------------------------
%%% File    : /Users/dfayram/Projects/concilium/elibs/resource_manager.erl
%%% Author  : David Fayram
%%%-------------------------------------------------------------------
-module(resource_manager).
-behaviour(gen_server).

%% API
-export([start_link/5,start/5,nodes/0,nodecount/0,change_spec/1,register_nodes/0,cycle/0,
         add_to_fountain/1, fountain_pool_for_node/1, add_node/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_fresh_nodes/0, stop_all_nodes/0]).

-record(state, {
                spec,
                preproc = fun() -> undefined end,
                postproc = fun(_) -> undefined end,
                nodespec = [],
                nodeapi = [],
                nodes = dict:new(),
                timeout = infinity,
                master = undefined
               }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------


start_link(Master, Nodes, Preproc, Postproc, Timeout) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Master, Nodes,Preproc,Postproc,Timeout], []).


start(Master, Nodes, Preproc, Postproc, Timeout) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Master, Nodes,Preproc,Postproc,Timeout], []).

% Returns the starting specification for this resource manager.
% @spec specification() -> {string(), int()}

% Returns a list of nodes maintained in this ResourceManager
% @spec nodes() -> [pid()]
nodes() -> gen_server:call(?MODULE, nodes).

% Returns a count of all nodes maintained currently.
% @spec nodecount() -> int()
nodecount() -> gen_server:call(?MODULE, nodecount).

% Changes the current nodecount. This may result in processes starting or stopping.
% @spec change_nodecount(int()) -> ok
change_spec(NewSpec) -> gen_server:cast(?MODULE, {change_spec, NewSpec}).

% Register the nodes with the master.
% @spec register_nodes() -> ok
register_nodes() -> gen_server:cast(?MODULE, register_nodes).

% Adds a node. Not for external use.
add_node(Node, Cmd) -> gen_server:cast(?MODULE, {add, Cmd, Node}).

% Restarts all processes managed by the current system. Good for un-wedging a wedged system.
% @spec cycle() -> ok
cycle() -> gen_server:cast(?MODULE, stop_all_nodes), gen_server:cast(?MODULE, start_fresh_nodes).

stop_all_nodes() -> gen_server:cast(?MODULE, stop_all_nodes).
start_fresh_nodes() -> gen_server:cast(?MODULE, start_fresh_nodes).

add_to_fountain(Node) -> resource_pool:add(fountain_pool_for_node(Node), Node), Node.
fountain_pool_for_node(Node) -> resource_fountain:pool_for_details(node_api:details(Node)).

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
init([Master, Nodes, Preproc, Postproc, Timeout]) ->
  process_flag(trap_exit, true),
  spawn_nodes(Nodes, Preproc, Timeout, dict:new()),
  {ok, #state{spec = Nodes,
              preproc = Preproc, 
              nodes = dict:new(), 
              postproc = Postproc,
              timeout = Timeout,
              master = Master}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(nodecount,_From,State) -> 
  {reply, length(dict:fetch_keys(State#state.nodes)), State};
handle_call(nodes, _From, State) ->
  {reply, dict:fetch_keys(State#state.nodes), State};
handle_call(spec, _From, State) -> 
  {reply, State#state.spec, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

% Node, Setup, Finisher, Registry

handle_cast({add, Cmd, Node}, State) -> 
  try resource_manager:add_to_fountain(Node) of
    _NoOneCares -> 
      link(Node), % We wuv woo!
      NewDict = add_node_record(Node, Cmd, State#state.nodes),
      {noreply, State#state{nodes=NewDict}}
  catch
    _:E -> 
      error_logger:error_msg("Tried to register fresh node ~p with fountain, but failed because: ~p", [Node, E]),
      {noreply, State}
  end;
handle_cast(start_fresh_nodes, State) -> 
  spawn_nodes(State#state.spec, State#state.preproc, State#state.timeout, State#state.nodes),
  {noreply, State};
handle_cast(stop_all_nodes, State) -> 
  NodeDict = State#state.nodes, 
  RenewNodes = fun(N) -> cease_node(N,State#state.postproc,NodeDict) end,
  lists:foreach(RenewNodes, dict:fetch_keys(NodeDict)),
  {noreply, State#state{nodes=dict:new()}};
handle_cast({change_spec, NewSpec}, State) -> 
  {noreply, State#state{spec=NewSpec}};
handle_cast(register_nodes, State) ->
  error_logger:info_msg("Reconnected to ~p, reregistering nodes now.~n", [State#state.master]),
  Nodes = State#state.nodes,
  lists:foreach(fun(X) -> add_to_fountain(X) end, dict:fetch_keys(Nodes)),
  error_logger:info_msg("All ports reregistered.~n"),
  {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) -> 
  case dict:is_key(Pid, State#state.nodes) of
    true ->
      error_logger:warning_msg("PortWrapper ~p was terminated due to: ~p. Restarting & Heating.", [Pid,Reason]),
      NewNodeDict = restart_dead_node(Pid,State#state.preproc, State#state.postproc,State#state.timeout,State#state.nodes),
      {noreply, State#state{nodes=NewNodeDict}};
    false -> 
      {noreply, State}
  end;
handle_info(Any,S) -> 
  error_logger:info_msg("Got INFO ~p~n", [Any]),
  {noreply, S}.

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

listify(X) when is_list(X) -> X;
listify(X) -> [X].

run_call({Module, Function}, Args) -> apply(Module, Function, listify(Args));
run_call(Function,Args) when is_function(Function) -> apply(Function, listify(Args)).

% @spec spawn_linked_node(Cmd, Call) -> {process(), Cmd}
spawn_unlinked_node(Cmd, Call, Timeout) -> 
  Port = port_wrapper:wrap(Cmd, Timeout),
  case run_call(Call, Port) of
    ok -> {Port, Cmd};
    fail -> {error, Cmd}
  end.

add_node_record(Node, Cmd, Registry) -> dict:store(Node, Cmd, Registry).
erase_node_record(Node, Registry) -> dict:erase(Node, Registry).

% @spec spawn_nodes(Nodes::[string()], Registerer::fun(), Registry::dict()) -> dict()
spawn_nodes({Cmd, Num}, Setup, Timeout, Registry) -> spawn_nodes(lists:duplicate(Num, Cmd), Setup, Timeout, Registry);
spawn_nodes(Nodes, Setup, Timeout, _Registry) when is_list(Nodes) ->
  F = fun(Cmd) -> spawn( 
    fun() -> 
      case spawn_unlinked_node(Cmd, Setup, Timeout) of
        {error, Cmd} -> 
          error_logger:error_msg("Failed to start node with command: ~n~p~n. Resource manager cannot function in this state, system is idle.~nPossible causes include a crash on startup or a timeout on startup. Make sure nodes can start on this machine!",
                                 [Cmd]);
        {Port, Cmd} -> resource_manager:add_node(Port, Cmd)
      end
    end ) 
  end,
  lists:foreach(F, Nodes).
  
restart_dead_node(Node, Setup, Finisher, Timeout, Registry) -> 
  % Node is dead, so it should be removed everywhere that 
  % cares about it, no need to explicitly remove it from
  % pools.
  apply(Finisher, [Node]),
  Cmd = dict:fetch(Node, Registry),
  NegReg = erase_node_record(Node, Registry),
  spawn_nodes([Cmd], Setup, Timeout, NegReg), 
  NegReg.

% @spec renew_node(proc(), dict()) -> dict()
cease_node(Node, Finisher, Registry) -> 
  try 
    resource_pool:remove(resource_manager:fountain_pool_for_node(Node),Node)
  catch
    _:X -> error_logger:error_msg("Failed to remove node ~p from resource fountain. Reason: ~p", [Node, X])
  end,
  unlink(Node),
  run_call(Finisher, [Node]),
  port_wrapper:shutdown(Node),
  erase_node_record(Node, Registry).
