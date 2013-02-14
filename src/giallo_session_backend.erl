-module(giallo_session_backend).

%%% Behaviour callbacks --------------------------------------------------------

-callback start() -> ok | {error, Reason} when
      Reason :: any().

-callback new(Sid) -> ok when
      Sid :: binary().

-callback get(Key, Sid, Default) -> Value when
      Key     :: any(),
      Sid     :: binary(),
      Default :: any(),
      Value   :: any().

-callback set(Key, Value, Sid) -> ok when
    Key   :: any(),
    Value :: any(),
    Sid   :: binary().

-callback exists(Sid) -> boolean() when
      Sid :: binary().

-callback clear(Sid) -> ok when
      Sid :: binary().
