%%%-------------------------------------------------------------------
%%% File    : /Users/dfayram/Projects/new_fuzed/elibs/fuzed_code_monitor.erl
%%% Author  :
%%%-------------------------------------------------------------------
-module(fuzed_code_monitor).
-behaviour(gen_server).

%% API
-export([modified_modules/0, reload_modified_modules/0,reload_modified_modules_for_all_nodes/0,
         reload_specified_modules/1, reload_ruby_code/0, reload_ruby_code_for_all_nodes/0,
         global_upgrade/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, start_link/0, start/0]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks & API
%%====================================================================

modified_modules() ->
  gen_server:call(?MODULE, modified_modules).

reload_modified_modules() ->
  gen_server:cast(?MODULE, reset).

reload_specified_modules(Modules) ->
  spawn(fun() -> [force_reload_module(M) || M <- Modules] end),
  ok.

reload_ruby_code() ->
  spawn(
    fun() ->
      try resource_manager:cycle()
      catch
        error:_ -> ok
      end
    end
  ),
  ok.

reload_ruby_code_for_all_nodes() ->
  reload_ruby_code(),
  [rpc:call(Node, ?MODULE, reload_ruby_code, []) || Node <- nodes()],
  nodes().

reload_modified_modules_for_all_nodes() ->
  ModSquad = modified_modules(),
  reload_modified_modules(), % locally
  [rpc:call(Node,?MODULE,reload_specified_modules,[ModSquad]) || Node <- nodes()],
  nodes(). % remotely

global_upgrade(ruby) -> reload_ruby_code_for_all_nodes();
global_upgrade(erlang) -> reload_modified_modules_for_all_nodes();
global_upgrade(all) -> global_upgrade(erlang), global_upgrade(ruby);
global_upgrade(_) -> global_upgrade(all).


%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
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
handle_call(modified_modules, _From, State) ->
    {reply, local_modified_modules(), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(reset, State) ->
  spawn(fun() -> timer:sleep(1000), [force_reload_module(M) || M <- mm()] end),
  {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

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

% Taken from http://www.erlang.org/ml-archive/erlang-questions/200411/msg00068.html
mm() ->
  local_modified_modules().

local_modified_modules() ->
  [M || {M, _} <-  code:all_loaded(), module_modified(M) == true].

module_modified(Module) ->
  case code:is_loaded(Module) of
    {file, preloaded} ->
      false;
    {file, Path} ->
      CompileOpts = proplists:get_value(compile, Module:module_info()),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src = proplists:get_value(source, CompileOpts),
      module_modified(Path, CompileTime, Src);
    _ ->
      false
  end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
  case find_module_file(Path) of
    false ->
      false;
    ModPath ->
      {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
      CompileOpts =  binary_to_term(CB),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src = proplists:get_value(source, CompileOpts),
      not (CompileTime == PrevCompileTime) and (Src == PrevSrc)
  end.

find_module_file(Path) ->
  case file:read_file_info(Path) of
    {ok, _} ->
      Path;
    _ ->
      %% may be the path was changed?
      case code:where_is_file(filename:basename(Path)) of
	non_existing ->
	  false;
	NewPath ->
	  NewPath
      end
  end.

force_reload_module(Module) ->
  code:purge(Module),
  code:load_file(Module).