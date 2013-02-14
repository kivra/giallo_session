-module(giallo_session_app).

-behaviour(application).

%%% Application callbacks
-export([start/2]).
-export([stop/1]).

%%% Application callbacks ------------------------------------------------------

start(_StartType, _StartArgs) ->
    Backend = giallo_session_config:backend(),
    ok = Backend:start(),
    giallo_session_sup:start_link().

stop(_State) ->
    ok.
