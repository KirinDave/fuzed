-module(port_wrapper).
-export([wrap/1, wrap/2, wrap_link/1, wrap_link/2, send/2, shutdown/1, rpc/2, pure_send/2]).
-author('Dave Fayram').

wrap(Command) ->
 spawn(fun() -> process_flag(trap_exit, true), Port = create_port(Command), loop(Port, infinity, Command) end).
wrap(Command, Timeout) ->
  spawn(fun() -> process_flag(trap_exit, true), Port = create_port(Command), loop(Port, Timeout, Command) end).

wrap_link(Command) ->
 spawn_link(fun() -> process_flag(trap_exit, true), Port = create_port(Command), link(Port), loop(Port, infinity, Command) end).
wrap_link(Command, Timeout) ->
  spawn_link(fun() -> process_flag(trap_exit, true), Port = create_port(Command), link(Port), loop(Port, Timeout, Command) end).

rpc(WrappedPort, Message) ->
  send(WrappedPort, Message),
  receive
    {WrappedPort, Result} -> Result
  after 15000 ->
    {WrappedPort, timed_out}
  end.

send(WrappedPort, Message) ->
  WrappedPort ! {self(), {command, term_to_binary(Message)}},
  WrappedPort.

pure_send(WrappedPort, Message) ->
  WrappedPort ! {self(), {just_send_a_command, term_to_binary(Message)}},
  WrappedPort.

shutdown(WrappedPort) ->
  WrappedPort ! shutdown,
  true.

create_port(Command) ->
  open_port({spawn, Command}, [{packet, 4}, nouse_stdio, exit_status, binary]).

loop(Port, Timeout, Command) ->
  receive
    noose ->
      port_close(Port),
      noose;
    shutdown ->
      port_close(Port),
      exit(shutdown);
    {Source, host} ->
      Source ! {Port, node()},
      loop(Port,Timeout,Command);
    {Source, heat} ->
      Port ! {self(), {command, term_to_binary(ping)}},
      Hot = term_to_binary(pong),
      receive
        {Port, {data, Hot}} ->
          Source ! {self(), hot}
      end,
      loop(Port, Timeout, Command);
    {Source, api} ->
      Port ! {self(), {command, term_to_binary(api)}},
      receive
        {Port, {data, Result}} ->
          {result, Api} = binary_to_term(Result),
          Source ! {self(), tuple_to_list(Api)}
      end,
      loop(Port,Timeout,Command);
    {Source, {command, Message}} ->
      Port ! {self(), {command, Message}},
      receive
        {Port, {data, Result}} ->
          DB = binary_to_term(Result),
          case DB of
            {last_result, X} ->
              Source ! {self(), {result, X}},
              port_close(Port),
              exit(last_result);
            Z ->
              Source ! {self(), Z}
          end
      after Timeout ->
        error_logger:error_msg("Port Wrapper ~p timed out in mid operation (~p)!~n", [self(),Message]),
        % We timed out, which means we need to close and then restart the port
        port_close(Port), % Should SIGPIPE the child.
        exit(timed_out)
      end,
      loop(Port,Timeout,Command);
    {_Source, {just_send_a_command, Message}} ->
      Port ! {self(), {command, Message}},
      loop(Port,Timeout,Command);
    {Port, {exit_status, _Code}} ->
      % Hard and Unanticipated Crash
      error_logger:error_msg( "Port closed! ~p~n", [Port] ),
      exit({error, _Code});
    {'EXIT',_Pid,shutdown} ->
      port_close(Port),
      exit(shutdown);
    Any ->
      error_logger:warning_msg("PortWrapper ~p got unexpected message: ~p~n", [self(), Any]),
      loop(Port, Timeout, Command)
  end.


% Local API
% TODO: Add retry detection
