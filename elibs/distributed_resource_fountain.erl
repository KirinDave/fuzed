-module(distributed_resource_fountain).
-behavior(gen_leader).

-compile(export_all).

-record(s, {details_pool_dict = dict:new(), 
                pool_details_dict = dict:new(),
                lookup_cache_dict = dict:new(),
                total_node_dict   = dict:new(),
                virgin            = true}).


init([]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Resource fountain (~p) starting...~n", node(self())),
  {ok, #s{}}.

% Called when someone becomes a leader. Really all it should do is dump state.
elected(State, _Election) ->
  error_logger:info_msg("DiReFo: ~p elected. Other nodes will be given state.~n",
                        node(self())),
  {ok, State, State}.

surrendered(State, LeaderState, _Election) ->
  reconstitute_state(State, LeaderState).

handle_DOWN(Node, State, _E) ->
    io:format("~p awknowledges that ~p went down!", [node(self()), Node]),
    {ok, State}. % No need to broadcast here.

handle_leader_call({add_node, Details, Node}, _From, State, _Election) when is_pid(Node) ->
  case find_pool_for_details(Details, State) of
    none ->
      % We'll create one, stash it, and tell everyone else to make a pool
      % and stash it.
      {Pool, UpdatedState} = create_pool_for_details(Details, State),
      resource_pool:add(Pool, Node),
      link(Node),
      {reply, Pool, 
              {make_pool_and_add_node, Details, Node}, 
              track_node(Node, UpdatedState)};
    Pool when is_pid(Pool) ->
      link(Node),
      {reply, Pool,
              {add_node_to_details_pool, Details, Node},
              track_node(Node)}
  end;
handle_leader_call() ->

      
