-module(ewoks_sup).

-behavior(supervisor).

-export([start_link/0]).  
-export([init/1]).  

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {#{strategy => rest_for_one, intensity => 10, period => 5}, 
        [#{id => ewoks_job_manager,
           start => {ewoks_job_manager, start_link, []}},
         #{id => ewoks_job_sup,
           start => {ewoks_job_sup, start_link, []}} %,
         #{id => ewoks_config,
           start => {ewoks_config, start_link, []}}
        ]}}.
