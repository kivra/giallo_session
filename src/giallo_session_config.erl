-module(giallo_session_config).

%%% API
-export([backend/0]).
-export([cookie_name/0]).

%%% API ------------------------------------------------------------------------

-spec backend() -> atom().
backend() ->
    get_env(backend, giallo_session_ets).

-spec cookie_name() -> binary().
cookie_name() ->
    get_env(cookie_name, <<"sid">>).

%%% Internal -------------------------------------------------------------------

%% @private
-spec get_env(any(), any()) -> any().
get_env(Key, Default) ->
    case application:get_env(giallo_session, Key) of
        undefined ->
            Default;
        Value ->
            Value
    end.
