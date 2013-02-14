-module(giallo_session_ets).

-behaviour(giallo_session_backend).

-define(TABLE, giallo_session).

%%% API
-export([start/0]).
-export([new/1]).
-export([get/3]).
-export([set/3]).
-export([exists/1]).
-export([clear/1]).

%%% API ------------------------------------------------------------------------

-spec start() -> ok.
start() ->
    ?TABLE = ets:new(?TABLE, [public, named_table]),
    ok.

-spec new(binary()) -> ok.
new(Sid) ->
    true = ets:insert_new(?TABLE, {Sid, []}),
    ok.

-spec get(any(), binary(), any()) -> any().
get(Key, Sid, Default) ->
    case get_all(Sid) of
        {ok, Session} ->
            case lists:keyfind(Key, 1, Session) of
                {Key, Value} ->
                    Value;
                false ->
                    Default
            end;
        error ->
            error(badarg) %% No running session
    end.

-spec set(any(), any(), binary()) -> ok.
set(Key, Value, Sid) ->
    case get_all(Sid) of
        {ok, Session0} ->
            Session1 = lists:keystore(Key, 1, Session0, {Key, Value}),
            ets:insert(?TABLE, {Sid, Session1}),
            ok;
        error ->
            error(badarg) %% No running session
    end.

-spec exists(binary()) -> boolean().
exists(Sid) ->
    case get_all(Sid) of
        {ok, _Session} ->
            true;
        error ->
            false
    end.

-spec clear(binary()) -> ok.
clear(Sid) ->
    true = ets:delete(?TABLE, Sid).

%%% Internal -------------------------------------------------------------------

%% @private
-spec get_all(binary()) -> {ok, proplists:proplist()} | error.
get_all(Sid) ->
    case ets:lookup(?TABLE, Sid) of
        [] ->
            error;
        [{Sid, Session}] ->
            {ok, Session}
    end.
