-module(response_error_logger).
-behaviour(gen_server).

% API Exports
-export([start_link/1, start/1, is_active/0, set_active/1, logfile_name/0,
         log_error/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {fname = "/p/log/fuzed/response_errors.log",
                active = false,
                fhandle}).

% External API
%...
start_link(Fname) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [Fname], []).

start(Fname) ->
  gen_server:start({global, ?MODULE}, ?MODULE, [Fname], []).

% Calls
is_active() ->
  gen_server:call({global, ?MODULE}, is_active).

set_active(Bool) ->
  gen_server:call({global, ?MODULE}, {set_active, Bool}).

logfile_name() ->
  gen_server:call({global, ?MODULE}, logfile_name).

% Casts
log_error({_Details, _ApiStr} = ReqTup, ErrorStr) ->
  gen_server:cast({global, ?MODULE}, {log_error, {ReqTup, ErrorStr}}).


% GenServer Internals
init([Fname]) ->
  case file:open(Fname, write) of
    {ok, Fh} ->
      error_logger:info_msg( "~p started! Will write to file: ~p", [?MODULE, Fname]),
      {ok, #state{fname=Fname, active=false, fhandle=Fh}};
    {error, Reason} ->
      error_logger:info_msg( "~p cannot start! Unable to write to file ~p:~n~p",
                             [?MODULE, Fname, Reason] ),
      #state{fname=Fname, active=false, fhandle=nofile}
  end.

% Calls. Most of these are just introspection.
handle_call(is_active, _From, State) ->
  {reply, State#state.active, State};
handle_call({set_active, Bool}, _From, State) when is_boolean(Bool) ->
  case State#state.fhandle of
    nofile -> {reply, {error, "No file available."}, State};
    _Else   -> {reply, Bool, State#state{active=Bool}}
  end;
handle_call(logfile_name, _From, State) ->
  {reply, State#state.fname, State}.

% Casts. Most of the meat goes here.
handle_cast({log_error, {RequestTuple, Error}}, State) ->
  case State#state.active of
    true -> write_error(RequestTuple, Error, State#state.fhandle);
    false -> nothing
  end,
  {noreply, State}.

handle_info(_,S) ->
  {noreply, S}.

terminate(_,S) ->
  file:close(S#state.fhandle),
  ok.

code_change(_, S, _) ->
  {ok, S}. % Whenever I write one of these I get this chill, like
               % I know that in the future I will regret it.


write_error({Details, ApiReq}, ErrorStr, FileHandle) ->
  io:format(FileHandle,
            "=== ERROR at ~s ===~n* For request: ~p~n* Details:~n~p~n* Reason:~n~p~n======~n",
            [now_string(), ApiReq, Details, ErrorStr]),
  file:sync(FileHandle).

now_string() ->
  {{_, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  io_lib:format("~p-~p (~p:~p:~p)", [Month, Day, Hour, Minute, Second]).


