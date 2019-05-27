-module(ewoks_config).
-behaviour(gen_server).

-export([
         start_link/0,
         init/1,
         log_dir/0,
         reload_configs/0,
         ewok_root_dir/0,
         handle_call/3,
         handle_cast/2
        ]).

-record(state, {
         ewok_root_dir
         }).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_dir() ->
  string:concat(?MODULE:ewok_root_dir(), "/logs/").

ewok_root_dir() -> "/tmp/ewok/".

reload_configs() ->
  {ok, [NewConfigs]} = file:consult(binary:list_to_bin([ewok_root_dir(), "ewoks_jobs.conf"])),
  % Compare against existing configs. Soon. TODO
  [ewoks_job_manager:insert_job(Conf) || Conf <- NewConfigs].

init([]) ->
  State = #state{},
  {ok, State}.

handle_call(_Msg, _From, State=#state{}) ->
  {reply, ok, State}.

handle_cast(_Msg, State=#state{}) ->
  {noreply, State}.
