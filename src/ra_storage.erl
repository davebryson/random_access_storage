%%% @doc
%%%  Common behavior to implement for different storage techniques
%%% @end
-module(ra_storage).

-callback write(
    Offset :: pos_integer(),
    Data :: binary(),
    State :: term()
) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback read(
    Offset :: pos_integer(),
    Length :: pos_integer(),
    State :: term()
) ->
    {ok, Result :: binary(), State :: term()} | {error, Reason :: term()}.

-callback del(
    Offset :: pos_integer(),
    Data :: binary(),
    State :: term()
) -> {ok, State :: term()} | {error, Reason :: term()}.

-callback truncate(
    Length :: pos_integer(),
    State :: term()
) -> {ok, State :: term()} | {error, Reason :: term()}.

-callback len(
    State :: term()
) -> {ok, pos_integer(), State :: term()} | {error, Reason :: term()}.

-callback is_empty(
    State :: term()
) -> {ok, boolean(), State :: term()} | {error, Reason :: term()}.

-callback sync_all(
    State :: term()
) -> {ok, State :: term()} | {error, Reason :: term()}.
