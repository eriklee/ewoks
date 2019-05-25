-module(ewoks_job_manager).
-behaviour(gen_server).

-include("job_spec.hrl").

-export([
   start_link/0,
   init/1,

   handle_call/3,
   handle_cast/2,
   terminate/2
  ]).

-record(state, {
  job_file :: ok,
  job_ets :: ets:ets()
 }).

-define(TABLE, ewoks_job_configs).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  Ets = ets:new(?TABLE, [protected, {keypos, #job_config.job_id}]),
  State = #state{job_ets=Ets},
  {ok, State}.

handle_call(_Msg, _From, State=#state{}) ->
  {reply, ok, State}.

handle_cast(_Msg, State=#state{}) ->
  {ok, State}.

terminate(_,_) -> ok.
