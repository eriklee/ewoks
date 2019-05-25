-module(ewoks_sup).

-behavior(supervisor).

-export([start_link/0]).  
-export([init/1]).  

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Args) ->
  {ok, {#{strategy => one_for_one, intensity => 1, period => 5}, 
        [#{id => ewoks_job_manager,
           start => {ewoks_job_manager, start_link, []}}  %,
       %  #{id => ewoks_perm_runner,
       %    start => {ewoks_perm_runner, start_link, []}},
       %  #{id => ewoks_cron_runner,
       %    start => {ewoks_cron_runner, start_link, []}},
       %  #{id => ewoks_control_tower,
       %    start => {ewoks_control_tower, start_link, []}}
        ]}}.

