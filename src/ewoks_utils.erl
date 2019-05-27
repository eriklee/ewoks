-module(ewoks_utils).

-export([
  kill_port/1,
  kill_port_TERM/1,
  kill_port_KILL/1,
  now_string/0
]).

kill_port(Port) ->
  case erlang:port_info(Port, os_pid) of
    undefined -> ok;
    {os_pid, _OsPid} ->
      kill_port_TERM(Port),
      kill_port(Port, 9)
  end.

kill_port(Port, 0) ->
  case erlang:port_info(Port, os_pid) of
    undefined -> ok;
    {os_pid, _OsPid} ->
      kill_port_KILL(Port)
  end;
kill_port(Port, Retries) ->
  timer:sleep(400),
  case erlang:port_info(Port, os_pid) of
    undefined -> ok;
    {os_pid, _OsPid} ->
      kill_port(Port, Retries - 1)
  end.

kill_port_TERM(Port) ->
  case erlang:port_info(Port, os_pid) of
    undefined -> ok;
    {os_pid, OsPid} ->
      os:cmd(io_lib:format("kill ~p", [OsPid]))
  end.

kill_port_KILL(Port) ->
  case erlang:port_info(Port, os_pid) of
    undefined -> ok;
    {os_pid, OsPid} ->
      os:cmd(io_lib:format("kill -9 ~p", [OsPid]))
  end.

now_string() ->
  calendar:system_time_to_rfc3339(erlang:system_time(millisecond),
                                  [{unit, millisecond}, {offset, "Z"}]).
