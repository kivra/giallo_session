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
