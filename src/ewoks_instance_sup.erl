-module(ewoks_instance_sup).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-include("job_spec.hrl").

-export([
  start_link/1,

  init/1,

  test_oo/0,
  test_persist/0
]).

test_oo() ->
  #job_config{
                 job_id = "echo",
                 src_path = "/usr/bin/",
                 cmd = "echo",
                 args = ["$PATH"],
                 env = [],
                 schedule = one_off}.

test_persist() ->
  #job_config{
                 job_id = "ping",
                 src_path = "/usr/bin/",
                 cmd = "ping",
                 args = ["localhost"],
                 env = [],
                 schedule = persistent}.

start_link(Job=#job_config{}) ->
  supervisor:start_link(?MODULE, [Job]).

init([Job=#job_config{}]) ->
  Logger = #{id=>job_log,
             start=>{ewoks_job_log,
                     start_link,
                     [ewoks_config:log_dir(),
                      Job#job_config.job_id]},
             restart=>permanent},
  JobMonitor = #{id=>job_monitor,
                 start=>{ewoks_job_runner,
                         start_link,
                         [Job, self()]},
                 restart=>permanent},
  ewoks_job_manager:register(Job#job_config.job_id),
  {ok, {#{strategy=> rest_for_one, intensity=>0}, [Logger, JobMonitor]}}.
