-module(master_beater).
-behaviour(gen_fsm).

-export([start_link/3]).

% events
-export([ping/0]).

% callbacks
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3, handle_sync_event/4]).

% states
-export([up/2, down/2, rejoin/2]).

-record(state, {master = undefined,
                up_retry = undefined,
                down_retry = undefined,
                ping_ref = undefined}).


start_link(Master, UpRetry, DownRetry) ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Master, UpRetry, DownRetry], []).


ping() ->
  gen_fsm:send_event(?MODULE, ping).


init([Master, UpRetry, DownRetry]) ->
  process_flag(trap_exit, true),
  link(global:whereis_name(resource_fountain)),
  Ref1 = pinger(UpRetry),
  {ok, up, #state{master = Master, up_retry = UpRetry, down_retry = DownRetry, ping_ref = Ref1}}.


up({ping, Ref}, StateData) when Ref =:= StateData#state.ping_ref ->
  case check_master_connection(StateData#state.master) of
    ok ->
      Ref1 = pinger(StateData#state.up_retry),
      {next_state, up, StateData#state{ping_ref = Ref1}};
    fail ->
      error_logger:info_msg("Master node is not connected. Going 'down'.~n"),
      Ref1 = pinger(500),
      {next_state, down, StateData#state{ping_ref = Ref1}}
  end;
up({ping, _Ref}, StateData) ->
  {next_state, up, StateData}.


down({ping, Ref}, StateData) when Ref =:= StateData#state.ping_ref ->
  case check_master_connection(StateData#state.master) of
    ok ->
      error_logger:info_msg("Master node is now connected. Going 'rejoin'.~n"),
      Ref1 = pinger(500),
      {next_state, rejoin, StateData#state{ping_ref = Ref1}};
    fail ->
      Ref1 = pinger(StateData#state.down_retry),
      {next_state, down, StateData#state{ping_ref = Ref1}}
  end;
down({ping, _Ref}, StateData) ->
  {next_state, down, StateData}.


rejoin({ping, Ref}, StateData) when Ref =:= StateData#state.ping_ref ->
  case check_master_responsive() of
    ok ->
      error_logger:info_msg("Master node is responsive. Going 'up'.~n"),
      link(global:whereis_name(resource_fountain)),
      resource_manager:register_nodes(),
      Ref1 = pinger(500),
      {next_state, up, StateData#state{ping_ref = Ref1}};
    fail ->
      error_logger:info_msg("Master node is not responsive. Retrying in 0.5 seconds.~n"),
      Ref1 = pinger(500),
      {next_state, down, StateData#state{ping_ref = Ref1}}
  end;
rejoin({ping, _Ref}, StateData) ->
  {next_state, rejoin, StateData}.


handle_info({'EXIT', Pid, Reason}, _StateName, StateData) ->
  error_logger:warning_msg("Pid ~p went away because ~p .~n", [Pid, Reason]),
  Ref1 = pinger(500),
  {next_state, down, StateData#state{ping_ref = Ref1}};
handle_info({nodeup, _Node, _Reason}, StateName, StateData) ->
  {next_state, StateName, StateData}.


handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.


handle_sync_event(_Event, _From, StateName, StateData) ->
  {next_state, StateName, StateData}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%%%%%%%%%%%%%%%

pinger(Delay) ->
  Ref = erlang:make_ref(),
  gen_fsm:send_event_after(Delay, {ping, Ref}),
  Ref.

check_master_connection(Node) ->
  case net_adm:ping(Node) of
    pong ->
      ok;
    pang ->
      fail
  end.

check_master_responsive() ->
  try resource_fountain:identity() of
    {ok, _Node} ->
      ok;
    _ ->
      fail
  catch
    _:_ ->
      fail
  end.
