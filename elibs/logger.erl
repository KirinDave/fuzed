%%%-------------------------------------------------------------------
%%% Author  : Tom Preston-Werner
%%%-------------------------------------------------------------------
-module(logger).
-behaviour(gen_server).

%% API
-export([start_link/1, enable_logging/1,
         disable_logging/1, list_details_selectors/0, list_pools/0,
         is_pool_logging/2, pool_created/1, pool_removed/1,
         node_joined/2, node_left/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(finfo,
  {
    log_file,
    name_file
  }
).

-record(state,
  {
    log_dir,
    details_selectors = [],
    pools_finfos = dict:new()
  }
).

%%====================================================================
%% API
%%====================================================================

enable_logging(Details) ->
  gen_server:call(?MODULE, {enable_logging, Details}).

disable_logging(Details) ->
  gen_server:call(?MODULE, {disable_logging, Details}).

list_details_selectors() ->
  gen_server:call(?MODULE, {list_details_selectors}).

list_pools() ->
  gen_server:call(?MODULE, {list_pools}).


is_pool_logging(Pool, Details) ->
  gen_server:call(?MODULE, {is_pool_logging, Pool, Details}).


pool_created(Pool) ->
  gen_server:cast(?MODULE, {pool_created, Pool}).

pool_removed(Pool) ->
  gen_server:cast(?MODULE, {pool_removed, Pool}).

node_joined(Pool, Node) ->
  gen_server:cast(?MODULE, {node_joined, Pool, Node}).

node_left(Pool, Node) ->
  gen_server:cast(?MODULE, {node_left, Pool, Node}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(LogDir) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [LogDir], []).

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
init([LogDir]) ->
  file:make_dir(LogDir),
  error_logger:info_msg("~p starting. Logs will be written to ~p~n", [?MODULE, LogDir]),
  {ok, #state{log_dir=LogDir}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({enable_logging, Details}, _From, State) ->
  DetailsSelectors = State#state.details_selectors,
  IsMember = lists:member(Details, DetailsSelectors),
  if
    IsMember ->
      error_logger:info_msg("Logging already enabled for those details~n", []),
      {reply, ok, State};
    true ->
      NewDetailsSelectors = lists:append(DetailsSelectors, [Details]),
      Pools = set_all_logging(Details, true),
      NewPoolsFinfos = init_finfos(Pools, State#state.pools_finfos, State#state.log_dir),
      {reply, ok, State#state{details_selectors=NewDetailsSelectors, pools_finfos=NewPoolsFinfos}}
  end;

handle_call({disable_logging, Details}, _From, State) ->
  DetailsSelectors = State#state.details_selectors,
  IsMember = lists:member(Details, DetailsSelectors),
  if
    IsMember ->
      NewDetailsSelectors = lists:delete(Details, DetailsSelectors),
      set_all_logging(Details, false),
      {reply, ok, State#state{details_selectors=NewDetailsSelectors}};
    true ->
      error_logger:info_msg("No logging has been enabled for those details~n", []),
      {reply, ok, State}
  end;

handle_call({list_details_selectors}, _From, State) ->
  {reply, State#state.details_selectors, State};

handle_call({list_pools}, _From, State) ->
  {reply, dict:fetch_keys(State#state.pools_finfos), State};

handle_call({is_pool_logging, Pool, Details}, _From, State) ->
  Pred = fun(Spec) ->
    Score = scoring:score_details(Spec, Details),
    if
      Score > 0 -> true;
      true      -> false
    end
  end,
  Logging = lists:any(Pred, State#state.details_selectors),
  case Logging of
    false ->
      {reply, false, State};
    true ->
      error_logger:info_msg("Enabling logging for pool ~p~n", [Pool]),
      NewPoolsFinfos = init_finfo(Pool, State#state.pools_finfos, State#state.log_dir, Details),
      {reply, true, State#state{pools_finfos=NewPoolsFinfos}}
  end;

handle_call(Any, _From, State) ->
  error_logger:info_msg("Unexpected call: ~p~n", [Any]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({pool_created, Pool}, State) ->
  Finfo = dict:fetch(Pool, State#state.pools_finfos),
  log_line(Finfo#finfo.log_file, "Pool created"),
  {noreply, State};

handle_cast({pool_removed, Pool}, State) ->
  Finfo = dict:fetch(Pool, State#state.pools_finfos),
  log_line(Finfo#finfo.log_file, "Pool removed"),
  {noreply, State};

handle_cast({node_joined, Pool, Node}, State) ->
  Finfo = dict:fetch(Pool, State#state.pools_finfos),
  log_line(Finfo#finfo.log_file, "Node joined: ~p ~p", [Node, node(Node)]),
  {noreply, State};

handle_cast({node_left, Pool, Node}, State) ->
  Finfo = dict:fetch(Pool, State#state.pools_finfos),
  log_line(Finfo#finfo.log_file, "Node left: ~p ~p", [Node, node(Node)]),
  {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Any, State) ->
  error_logger:info_msg("Unexpected info: ~p~n", [Any]),
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

set_all_logging(Details, Logging) ->
  Matches = resource_fountain:rate_all_pools(Details),
  PoolExtractFun = fun(Match) ->
    {MatchDetails, _Score} = Match,
    resource_fountain:pool_for_details(MatchDetails)
  end,
  SetLoggingFun = fun(Pool) ->
    case Logging of
      false -> error_logger:info_msg("Disabling logging for pool ~p~n", [Pool]);
      true -> error_logger:info_msg("Enabling logging for pool ~p~n", [Pool])
    end,
    resource_pool:set_logging(Pool, Logging)
  end,
  Pools = lists:map(PoolExtractFun, Matches),
  lists:foreach(SetLoggingFun, Pools),
  Pools.

init_finfo(Pool, PoolsFinfos, LogDir, Details) ->
  Hash = erlang:phash2(Details),
  case dict:find(Hash, PoolsFinfos) of
    {ok, _Finfo} ->
      PoolsFinfos;
    error ->
      Finfo = init_files(Hash, Details, LogDir),
      dict:store(Pool, Finfo, PoolsFinfos)
  end.

init_finfos(Pools, PoolsFinfos, LogDir) ->
  InitFun = fun(Pool, Dict) ->
    Details = resource_pool:details(Pool),
    Hash = erlang:phash2(Details),
    case dict:find(Hash, Dict) of
      {ok, _Finfo} -> ok;
      error ->
        Finfo = init_files(Hash, Details, LogDir),
        dict:store(Pool, Finfo, Dict)
    end
  end,
  lists:foldl(InitFun, PoolsFinfos, Pools).

init_files(Hash, Details, LogDir) ->
  StringHash = erlang:integer_to_list(Hash),
  LogFile = filename:join(LogDir, StringHash ++ ".log"),
  NameFile = filename:join(LogDir, StringHash ++ ".details"),
  case file:open(NameFile, write) of
    {ok, Fh} ->
      error_logger:info_msg("Creating Logger NameFile: ~p~n", [NameFile]),
      io:format(Fh, "~p", [Details]),
      file:close(Fh);
    {error, Reason} ->
      error_logger:info_msg("Unable to create Logger NameFile ~p: ~p~n", [NameFile, Reason])
  end,
  #finfo{log_file=LogFile, name_file=NameFile}.

log_line(LogFile, Line) ->
  log_line(LogFile, Line, []).

log_line(LogFile, Line, Data) ->
  case file:open(LogFile, [write, append]) of
    {ok, Fh} ->
      io:format(Fh, "[~s] " ++ Line ++ "~n", [now_string()] ++ Data),
      file:close(Fh);
    {error, Reason} ->
      error_logger:info_msg("Unable to log to ~p: ~p~n", [LogFile, Reason])
  end.

now_string() ->
  {{_, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  io_lib:format("~p-~p (~p:~p:~p)", [Month, Day, Hour, Minute, Second]).