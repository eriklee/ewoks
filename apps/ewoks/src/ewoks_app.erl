-module(ewoks_app).
-behavior(application).

-export([
   start/2,
   stop/1
  ]).

start(_Type, _Args) ->
  ewoks_sup:start_link().

stop(_) -> ok.
