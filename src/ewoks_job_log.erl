-module(ewoks_job_log).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

-export([
         start_link/2,
         log/2,

         init/1,
         handle_call/3,
         handle_continue/2,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-record(state, {
          root_file_path :: binary(),
          file_name :: binary(),
          date :: iolist(),
          file
         }).

start_link(LogPath, Filename) ->
  gen_server:start_link(?MODULE, [LogPath, Filename], []).

log(LoggerPid, Msg) ->
  gen_server:cast(LoggerPid, {log, self(), Msg}).

init([LogPath, Filename]) ->
  % We need to sync and close the file on exit
  process_flag(trap_exit, true),
  Date = to_date_string(ewoks_utils:now_string()),
  State=#state{root_file_path=binary:list_to_bin(LogPath),
               file_name=binary:list_to_bin(Filename),
               date=Date, file=undefined},
  {ok, State, {continue, init}}.

handle_call(Msg, _From, State=#state{}) ->
  io:format("~p", [Msg]),
  {reply, ok, State}.

handle_continue(init, State=#state{}) ->
  FilePath = State#state.root_file_path,
  FileName = State#state.file_name,
  LogPath = full_path(FilePath, FileName), 
  ok = filelib:ensure_dir(LogPath),
  ?LOG_INFO("setting up logger file: ~p", [LogPath]),
  State2 = #state{} = check_existing_file(LogPath, State),
  {noreply, State2}.

handle_cast({log, FromPid, Msg}, State=#state{date=Date}) ->
  Now = ewoks_utils:now_string(),
  NowDate = to_date_string(Now),
  State2 = #state{file=File} = maybe_rotate_file(NowDate, Date, State),
  file:write(File, io_lib:format("~s ~p ~s~n", [Now, FromPid, Msg])),
  {noreply, State2}.

handle_info(Msg, State=#state{}) ->
  io:format("~p", [Msg]),
  {noreply, State}.

terminate(Reason, _State=#state{file=File}) ->
  file:sync(File),
  file:close(File),
  Reason.

maybe_rotate_file(Now, Now, State=#state{}) -> State;
maybe_rotate_file(_Now, Yesterday,
                  State=#state{file=File,
                               root_file_path=FilePath,
                               file_name=FileName}) ->
  ok = file:sync(File),
  ok = file:close(File),
  FullName = full_path(FilePath, FileName),
  YesterdaysName = dated_file_name(Yesterday, FullName),

  ok = file:rename(FullName, YesterdaysName),

  {ok, File} = file:open(FullName, [append, delayed_write, raw, binary]),
  State#state{file=File}.

to_date_string(Datetime) -> string:slice(Datetime, 0, 10).

full_path(FilePath, FileName) ->
  binary:list_to_bin([FilePath, <<"/">>, FileName]).

dated_file_name(DateString, FilePath) ->
  % Want to turn /blah/foo.log into /blah/foo.<DATE>.log
  case string:split(FilePath, ".", trailing) of
    [FilePath] -> unicode:characters_to_binary([FilePath, ".", DateString]);
    [Init, Extension] -> unicode:characters_to_binary([Init, ".", DateString, ".", Extension])
  end.

check_existing_file(FullPath, State=#state{}) ->
  case filelib:file_size(FullPath) of
    0 ->
      {ok, File} = file:open(FullPath, [append, delayed_write, raw, binary]),
      State#state{file=File};
    _N ->
      {ok, LastFile} = file:open(FullPath, [read, raw]),
      {ok, FileDate} = file:read(LastFile, 10),
      Now = to_date_string(ewoks_utils:now_string()),
      State2 = maybe_rotate_file(Now, FileDate, State#state{file=LastFile}),
      case State2#state.file of
        LastFile ->
          file:close(LastFile),
          {ok, File} = file:open(FullPath, [append, delayed_write, raw, binary]),
          State2#state{file=File};
        _ ->
          State2
      end
  end.
