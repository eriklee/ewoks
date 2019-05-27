-module(ewoks_job_sup).
-behavior(supervisor).

-include("src/job_spec.hrl").

-export([start_link/0, 
         start_child/1,
         stop_child/1,
         init/1]).  

start_link() ->
  supervisor:start_link({local, ?MODULE}, []).

stop_child(JobId) ->
  supervisor:terminate_child(?MODULE, JobId).

start_child(JobConfig=#job_config{job_id=JobId}) ->
  supervisor:terminate_child(?MODULE, JobId),
  supervisor:start_child(?MODULE, #{id=> JobId,
                                    start=> {ewoks_instance_sup, start_link, JobConfig},
                                    restart=> temporary,
                                    type=>supervisor
                                   }).

init(_Args) ->
  {ok, {#{strategy => one_for_one, intensity => 10, period => 5}, 
        []}}.
