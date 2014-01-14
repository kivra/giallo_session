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
-module(giallo_session).

%%% API
-export([new/1, new/2]).
-export([get/2, get/3]).
-export([set/3]).
-export([exists/1]).
-export([clear/1]).

-define(BACKEND, (giallo_session_config:backend())).

%%% API ------------------------------------------------------------------------

%% Create a new session, returning an updated request containing a
%% session cookie in the response section.
%% @see exists/1
new(Req) ->
    new(Req, []).

new(Req, CookieOpts) ->
    SidName = giallo_session_config:cookie_name(),
    Sid = generate_sid(),
    ok = ?BACKEND:new(Sid),
    giallo_session_reaper:register(Sid),
    Req1 = cowboy_req:set_meta(SidName, Sid, Req),
    %% Prepare CookieOpts values
    {HTTPS, Req2} = is_https(Req1),
    DefaultCookieOpts = lists:ukeysort(1, [{path, <<"/">>}, {http_only, true},
                                           {secure, HTTPS}]),
    ConfCookieOpts = lists:ukeysort(1, giallo_session_config:cookie_options()),
    ParamCookieOpts = lists:ukeysort(1, CookieOpts),
    %% Merge CookieOpts (Parameter overrides Config overrides Defaults)
    CookieOpts1 = lists:ukeymerge(1, ConfCookieOpts, DefaultCookieOpts),
    CookieOpts2 = lists:ukeymerge(1, ParamCookieOpts, CookieOpts1),
    %% These may be set to false to override Config/Default values,
    %% but Cowboy can't handle that, so strip them if false
    CookieOptsD1 = lists:delete({http_only, false}, CookieOpts2),
    CookieOptsD2 = lists:delete({secure, false}, CookieOptsD1),
    cowboy_req:set_resp_cookie(SidName, Sid, CookieOptsD2, Req2).

%% @doc Gets the value of a session property.
%% If no session exists, the behaviour is undefined.
%% @see exists/1
get(Key, Req, Default) ->
    case get_sid(Req) of
        undefined ->
            Default;
        Sid ->
            keep_alive(Sid),
            ?BACKEND:get(Key, Sid, Default)
    end.

%% @doc Gets the value of a session property, defaulting to undefined.
%% If no session exists, the behaviour is undefined.
%% @see exists/1
%% @equiv get(Key, Req, undefined).
get(Key, Req) ->
    get(Key, Req, undefined).

%% @doc Sets the value of a session property.
%% If no session exists, the behaviour is undefined.
%% @see exists/1
set(Key, Value, Req) ->
    case get_sid(Req) of
        undefined ->
            error(badarg);
        Sid ->
            keep_alive(Sid),
            ?BACKEND:set(Key, Value, Sid)
    end.

%% @doc Checks whether a session exists.
exists(Req) ->
    case get_sid(Req) of
        undefined ->
            false;
        Sid ->
            keep_alive(Sid),
            ?BACKEND:exists(Sid)
    end.

%% @doc Clears the session associated with this request.
%% The session cookie is not removed.
clear(Req) ->
    case get_sid(Req) of
        undefined ->
            ok;
        Sid ->
            giallo_session_reaper:unregister(Sid),
            ?BACKEND:clear(Sid)
    end.

%%% Internal -------------------------------------------------------------------

%% @private
get_sid(Req) ->
    SidName = giallo_session_config:cookie_name(),
    case cowboy_req:meta(SidName, Req) of
        {undefined, Req1} ->
            case cowboy_req:cookie(SidName, Req1) of
                {undefined, _Req} -> undefined;
                {Sid, _Req}       -> Sid
            end;
        {Sid, _Req} ->
            Sid
    end.

%% @private
generate_sid() ->
    generate_fragment(32).

%% @private
-spec generate_fragment(N :: integer()) -> binary().
generate_fragment(0) ->
    <<>>;
generate_fragment(N) ->
    Rand = base64:encode(crypto:rand_bytes(N)),
    Frag = << <<C>> || <<C>> <= <<Rand:N/bytes>>, is_alphanum(C) >>,
    <<Frag/binary, (generate_fragment(N - byte_size(Frag)))/binary>>.

%% @private
-spec is_alphanum(Char :: char()) -> boolean().
is_alphanum(C) when C >= $0 andalso C =< $9 ->
    true;
is_alphanum(C) when C >= $a andalso C =< $z ->
    true;
is_alphanum(C) when C >= $A andalso C =< $Z ->
    true;
is_alphanum(_) ->
    false.

%% @private
is_https(Req) ->
    case cowboy_req:header(<<"x-forwarded-proto">>, Req) of
        {undefined, Req1} ->
            %% Cowboy doesn't currently provide direct access to the
            %% transport, so get it via host_url()
            case cowboy_req:host_url(Req1) of
                {<<"https", _Rest/binary>>, Req2} -> {true, Req2};
                {_, Req2} -> {false, Req2}
            end;
        {Proto, Req1} ->
            {Proto =:= <<"https">>, Req1}
    end.

%% @private
-spec keep_alive(binary()) -> ok.
keep_alive(Sid) ->
    giallo_session_reaper:touch(Sid),
    ok.
