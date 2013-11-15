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
-module(giallo_session_reaper).

-behaviour(gen_server).

%%% API
-export([start_link/0]).
-export([register/1]).
-export([unregister/1]).
-export([touch/1]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE). 
-define(REAP_INTERVAL, 60000). %% Run reaper every 60s
-define(TIMER_MESSAGE, tick).
-define(BACKEND, (giallo_session_config:backend())).

-record(state, {
          live :: dict() % Dictionary of live sessions mapped to activity times
         }).

%%% API ------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Registers a new session, identified by Sid, with the reaper.
%% Marks it as active at the moment of the call.
register(Sid) ->
    gen_server:cast(?SERVER, {touch, Sid}). %% Currently equivalent to touch!

%% @doc Unregisters the session identified by Sid.
%% The session is assumed to be removed, so the reaper will
%% no longer monitor it for TTL expiry.
unregister(Sid) ->
    gen_server:cast(?SERVER, {unregister, Sid}).

%% @doc Marks the session identified by Sid as active at this moment.
touch(Sid) ->
    gen_server:cast(?SERVER, {touch, Sid}).

%%% gen_server callbacks -------------------------------------------------------

init([]) ->
    {ok, _TimerRef} = timer:send_interval(?REAP_INTERVAL, ?TIMER_MESSAGE),
    {ok, #state{live=dict:new()}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({Request, Sid}, #state{live=Live0}=State0) ->
    Live1 = case Request of
                touch ->
                    dict:store(Sid, now(), Live0);
                unregister ->
                    dict:erase(Sid, Live0)
            end,
    State1 = State0#state{live=Live1},
    {noreply, State1}.

handle_info(?TIMER_MESSAGE, #state{live=Live0}=State0) ->
    {ok,TTL} = giallo_session_config:ttl(),
    Live1 = dict:fold(
              fun(Sid, Time, Live) ->
                      case seconds_since(Time) > TTL of
                          true ->
                              ?BACKEND:clear(Sid),
                              dict:erase(Sid, Live);
                          false ->
                              Live % No change
                      end
              end,
              Live0,
              Live0),
    State1 = State0#state{live=Live1},
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal -------------------------------------------------------------------

-spec seconds_between(erlang:timestamp(), erlang:timestamp())
                     -> non_neg_integer().
seconds_between(Time1, Time0) ->
    round(timer:now_diff(Time1, Time0) / 1000000).

-spec seconds_since(erlang:timestamp()) -> non_neg_integer().
seconds_since(Time) ->
    seconds_between(now(), Time).

%%% Tests ----------------------------------------------------------------------

-ifdef(TEST).

seconds_between_test() ->
    Time1 = {1363,703320,38386},
    Time0 = {1363,703260,38386},
    ?assertEqual(60, seconds_between(Time1, Time0)).

-endif.
