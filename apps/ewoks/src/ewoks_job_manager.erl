-module(ewoks_job_manager).
-behaviour(gen_server).

-include("job_spec.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
   start_link/0,
   register/1,
   start_job/1,
   stop_job/1,
   insert_job/1,
   delete_job/1,
   init/1,

   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2
  ]).

-record(state, {
  job_ets :: ets:ets(),
  job_instance_ets :: ets:ets()
 }).

-define(TABLE, ewoks_job_configs).
-define(INSTANCE_TABLE, ewoks_job_instances).
-define(DEFAULT_BACKOFF, 50). % Default backoff in milliseconds

-record(job_instance, {
          job_id :: string(),
          job_sup_pid :: pid(),
          job_sup_ref :: reference(),
          job_last_failure,
          job_backoff = ?DEFAULT_BACKOFF % Backoff timer (ms)
         }).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(JobId) ->
  gen_server:cast(?MODULE, {register, JobId, self()}).

start_job(JobId) ->
  gen_server:cast(?MODULE, {start_job, JobId}).

stop_job(JobId) ->
  gen_server:cast(?MODULE, {stop_job, JobId}).

insert_job(JobConfig=#job_config{}) ->
  gen_server:cast(?MODULE, {insert_job, JobConfig}).

delete_job(JobId) ->
  gen_server:cast(?MODULE, {delete_job, JobId}).

init([]) ->
  Ets  = ets:new(?TABLE, [named, protected, {keypos, #job_config.job_id}]),
  Ets2 = ets:new(?TABLE, [named, protected, {keypos, #job_instance.job_id}]),
  State = #state{job_ets=Ets, job_instance_ets=Ets2},
  {ok, State}.

handle_call(_Msg, _From, State=#state{}) ->
  {reply, ok, State}.

handle_cast({register, JobId, Pid}, State=#state{}) ->
  ?LOG_INFO("Registering ~p to ~p", [JobId, Pid]),
  handle_register(JobId, Pid, State#state.job_instance_ets),
  {noreply, State};
handle_cast({start_job, JobId}, State=#state{}) ->
  case ets:lookup(State#state.job_ets, JobId) of
    [Config=#job_config{}] ->
      ets:insert(State#state.job_ets, Config#job_config{status=restarting}),
      ewoks_job_sup:start_child(Config);
    _ ->
      ?LOG_WARNING("Asked to start job ~p, but don't have the config", [JobId])
  end,
  {noreply, State};
handle_cast({stop_job, JobId}, State=#state{}) ->
  case ets:lookup(State#state.job_ets, JobId) of
    [Config=#job_config{status=running}] ->
      ets:insert(State#state.job_ets, Config#job_config{status=stopped}),
      ewoks_job_sup:stop_child(Config);
    _ ->
      ?LOG_WARNING("Asked to start job ~p, but don't have the config", [JobId])
  end,
  {noreply, State};
handle_cast({insert_job, JobConfig=#job_config{job_id=JobId}}, State=#state{}) ->
  case ets:lookup(State#state.job_ets, JobId) of
    [JobConfig=#job_config{status=running}] ->
      ok;
    [Config=#job_config{status=running}] ->
      ?LOG_WARNING("Adding a job (~p) that already existed. Stopping the old one.", [JobId]),
      ewoks_job_sup:start_child(Config);
    _ when JobConfig#job_config.status /= stopped ->
      ewoks_job_sup:start_child(JobConfig)
  end,
  ets:insert(State#state.job_ets, JobConfig),
  {noreply, State};
handle_cast({delete_job, JobId}, State=#state{}) ->
  case ets:lookup(State#state.job_ets, JobId) of
    [Config=#job_config{status=running}] ->
      ewoks_job_sup:stop_child(Config);
    _ ->
      ?LOG_WARNING("Asked to delete job ~p, but don't have the config", [JobId])
  end,
  ets:delete(State#state.job_ets, JobId),
  {noreply, State};
handle_cast(_Msg, State=#state{}) ->
  {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Info}, State=#state{}) ->
  ?LOG_INFO("Recieved DOWN message from ~p with reason ~p", [Pid, Info]),
  handle_down(MonitorRef, Pid, Info, State),
  {noreply, State}.

terminate(_,_) -> ok.

handle_register(JobId, Pid, #state{job_ets=JobEts,
                                   job_instance_ets=InstanceEts}) ->
  MRef = erlang:monitor(process, Pid),
  maybe_stop(JobId, JobEts),
  case ets:lookup(InstanceEts, JobId) of
    [] ->
      ets:insert(InstanceEts,
                 #job_instance{job_id=JobId,
                               job_sup_pid=Pid,
                               job_sup_ref=MRef});
    [JobInstance=#job_instance{}] ->
      ets:insert(InstanceEts,
                 JobInstance#job_instance{
                   job_sup_ref=MRef,
                   job_sup_pid=Pid})
  end.

handle_down(MonitorRef, Pid, _Reason, State=#state{job_instance_ets=InstanceEts}) ->
  case ets:match(InstanceEts,
                 #job_instance{job_id='_',
                               job_sup_pid=Pid,
                               job_sup_ref=MonitorRef,
                               job_last_failure='_',
                               job_backoff='_'}) of
    [Instance=#job_instance{job_id=JobId}] ->
      Time = os:system_time(millisecond),
      NewInstance = maybe_reschedule_job(JobId, Time,
                           Instance#job_instance.job_last_failure, 
                           Instance#job_instance.job_backoff,
                           State),
      ets:insert(InstanceEts, NewInstance);
    _ ->
      ?LOG_INFO("Ref ~p on pid ~p not found, ignoring", [MonitorRef, Pid])
  end.

maybe_reschedule_job(JobId, Time, LastFailure, Backoff, State=#state{}) ->
  NewBackoff = new_backoff(Time, LastFailure, Backoff),
  case ets:lookup(State#state.job_ets, JobId) of
    [Config=#job_config{schedule=permanent, status=running}] ->
      timer:apply_after(NewBackoff, ?MODULE, start_job, [JobId]),
      ets:insert(State#state.job_ets, Config#job_config{status=restarting});
    _ ->
      ok
  end,
  #job_instance{job_id=JobId, job_last_failure=Time, job_backoff=NewBackoff}.

new_backoff(_Time, undefined, Backoff) -> Backoff;
new_backoff(Time, LastFailure, Backoff)
  when (Time - LastFailure) < Backoff -> Backoff * 2;
new_backoff(_Time, _LastFailure, Backoff) -> Backoff.

maybe_stop(JobId, JobEts) ->
  case ets:lookup(JobEts, JobId) of
    [#job_config{status=stopped}] ->
      ewoks_job_sup:stop_child(JobId);
    _ ->
      ets:update(JobEts, JobId, [{#job_config.status, running}])
  end.
