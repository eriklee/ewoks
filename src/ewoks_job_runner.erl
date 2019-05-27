-module(ewoks_job_runner).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("job_spec.hrl").

-export([
         start_link/2,
         init/1,
         handle_call/3,
         handle_continue/2,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-record(state, {
          child_spec :: #job_config{},
          job_id,
          child_port,
          log_pid :: pid()
         }).

start_link(Job=#job_config{}, SupPid) ->
  gen_server:start_link(?MODULE, [Job, SupPid], []).

init([ChildSpec=#job_config{job_id=JobId}, SupPid]) ->
  % We want to have a chance to clean up children
  process_flag(trap_exit, true),
  State = #state{child_spec=ChildSpec, job_id=JobId},
  {ok, State, {continue, {init, SupPid}}}.

handle_continue({init, SupPid}, State=#state{child_spec=Job=#job_config{}}) ->
  ?LOG_INFO("Starting job: ~p", [Job#job_config.job_id]),
  JobPath = binary:list_to_bin([Job#job_config.src_path, Job#job_config.cmd]),
  Child = open_port({spawn_executable, JobPath},
                    [{args, Job#job_config.args},
                     {env, Job#job_config.env},
                     {cd, "/usr/bin"},
                     binary, stderr_to_stdout,
                     exit_status, {line, 4096}]),
  [{job_log, LogPid, _, _}] = [L || L={job_log, _, _, _} <-
                                    supervisor:which_children(SupPid)],
  {noreply, State#state{child_port=Child, log_pid=LogPid}}.


handle_call(stop, From, State=#state{child_port=ChildPort, job_id=JobId}) ->
  Res = ewoks_utils:kill_port(ChildPort),
  ?LOG_INFO("Asked to stop process ~p by ~p", [JobId, From]),
  {stop, normal, Res, State};
handle_call(Msg, _From, State=#state{}) ->
  io:format("~p", [Msg]),
  {reply, ok, State}.

handle_cast(Msg, State=#state{}) ->
  io:format("~p", [Msg]),
  {noreply, State}.

handle_exit_status({exit_status, 0}, State=#state{}) ->
  ?LOG_INFO("Process ended successfully"),
  {stop, normal, State};
handle_exit_status({exit_status, ES}, State=#state{child_spec=Job}) ->
  ?LOG_WARNING("Process ended badly! ~p", [ES]),
  JobId = Job#job_config.job_id,
  {stop, {process_crash, JobId, ES}, State}.

handle_info({Port, ES={exit_status, _}}, State=#state{child_port=Port}) ->
  handle_exit_status(ES, State);

handle_info(Msg, State=#state{log_pid=LogPid}) ->
  ewoks_job_log:log(LogPid, io_lib:format("~p", [Msg])),
  {noreply, State}.

terminate(_,State=#state{child_port=ChildPort}) ->
  ewoks_utils:kill_port(ChildPort),
  {ok, State}.
