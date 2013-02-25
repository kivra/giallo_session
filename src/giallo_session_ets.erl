%% ----------------------------------------------------------------------------
%%
%% giallo_session: Session manager for Giallo
%%
%% Copyright (c) 2013 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% ----------------------------------------------------------------------------
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
             undefined %% No running session
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
    true = ets:delete(?TABLE, Sid),
    ok.

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
