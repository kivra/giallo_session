-module(giallo_session).

%%% API
-export([new/1]).
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
    SidName = giallo_session_config:cookie_name(),
    Sid = generate_sid(),
    ok = ?BACKEND:new(Sid),
    cowboy_req:set_resp_cookie(SidName, Sid, [], Req).

%% @doc Gets the value of a session property.
%% If no session exists, the behaviour is undefined.
%% @see exists/1
get(Key, Req, Default) ->
    case get_sid(Req) of
        undefined ->
            error(badarg);
        Sid ->
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
            ?BACKEND:set(Key, Value, Sid)
    end.

%% @doc Checks whether a session exists.
exists(Req) ->
    case get_sid(Req) of
        undefined ->
            false;
        Sid ->
            ?BACKEND:exists(Sid)
    end.

%% @doc Clears the session associated with this request.
%% The session cookie is not removed.
clear(Req) ->
    case get_sid(Req) of
        undefined ->
            error(badarg);
        Sid ->
            ?BACKEND:clear(Sid)
    end.

%%% Internal -------------------------------------------------------------------

%% @private
get_sid(Req) ->
    SidName = giallo_session_config:cookie_name(),
    case cowboy_req:cookie(SidName, Req) of
        {undefined, _Req} ->
            undefined;
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
