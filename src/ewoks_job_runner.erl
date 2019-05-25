-module(ewoks_job_runner).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("job_spec.hrl").

-export([
         test_persist/0,
         test_oo/0,

         start_link/1,
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
          child_port
         }).

test_oo() ->
  ChildSpec = #job_config{
                 job_id = "echo",
                 src_path = "/usr/bin/",
                 cmd = "echo",
                 args = ["$PATH"],
                 env = [],
                 schedule = one_off},
  ?MODULE:start_link(ChildSpec).

test_persist() ->
  ChildSpec = #job_config{
                 job_id = "ping",
                 src_path = "/usr/bin/",
                 cmd = "ping",
                 args = ["localhost"],
                 env = [],
                 schedule = persistent},
  ?MODULE:start_link(ChildSpec).

start_link(Job=#job_config{}) ->
  gen_server:start_link(?MODULE, [Job], []).

init([ChildSpec=#job_config{job_id=JobId}]) ->
  % We want to have a chance to clean up children
  process_flag(trap_exit, true),

  State = #state{child_spec=ChildSpec, job_id=JobId},
  {ok, State, {continue, init}}.

handle_continue(init, State=#state{child_spec=Job=#job_config{}}) ->
  io:format("Starting job: ~p", [Job#job_config.job_id]),
  JobPath = binary:list_to_bin([Job#job_config.src_path, Job#job_config.cmd]),
  Child = open_port({spawn_executable, JobPath},
                    [{args, Job#job_config.args},
                     {env, Job#job_config.env},
                     {cd, "/usr/bin"},
                     binary, stderr_to_stdout,
                     exit_status, {line, 4096}]),
  {noreply, State#state{child_port=Child}}.


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
  io:format("Process ended successfully"),
  {stop, normal, State};
handle_exit_status({exit_status, ES}, State=#state{child_spec=Job}) ->
  io:format("Process ended badly! ~p", [ES]),
  JobId = Job#job_config.job_id,
  {stop, {process_crash, JobId, ES}, State}.

handle_info({Port, ES={exit_status, _}}, State=#state{child_port=Port}) ->
  handle_exit_status(ES, State);

handle_info(Msg, State=#state{}) ->
  io:format("~p", [Msg]),
  {noreply, State}.

terminate(_,State=#state{child_port=ChildPort}) ->
  ewoks_utils:kill_port(ChildPort),
  {ok, State}.
