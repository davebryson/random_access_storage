%%% @doc
%%%  Random Access Memory Storage
%%%
%%%  Memory buffers are used to store data of a configured size.  When the incoming
%%%  data is larger than a buffer, a new one is automatically created.
%%%
%%%  The biggest challenge here is fitting data into the correct buffer(s).
%%%  Instead of using 1 huge buffer for the data, we use several small buffers
%%%  indexed by a index number to store the data.
%%%
%%%  When writing data, if the offset to write at and the length of the incoming
%%%  data is less than or equal to the buffer size, we simply copy it all to that given page/buffer.
%%%  But, what if the incoming data is larger than a single buffer, possibly
%%%  spanning multiple buffers?
%%%
%%%  Visualize: Buffer size = 4
%%%```
%%%  Offset   0
%%%  Data:   [a,a,a,a]
%%%  Buffer: [ , , , ]
%%%
%%%  Offset       2
%%%  Data:       [a,a, a,a]
%%%  Buffer: [ , , , ][ , , , ]
%%%
%%%  Offset         3
%%%  Data:         [a, a,a,a,b, b,b,b,c, c]
%%%  Buffer: [ , , , ][ , , , ][ , , , ][ , , , ]
%%%
%%%  Offset   0  1
%%%  Data:   [a][a]
%%%  Buffer: [ ,  ,  ,  ]
%%%'''
%%% @end
-module(ra_storage_memory).

-behaviour(ra_storage).

-export([
    new/0,
    new/1,
    write/3,
    read/3,
    get_page/2,
    del/3,
    len/1
]).

-define(DEFAULT_PAGE_SIZE, (1024 * 1024)).

-type state() :: {MemoryPager :: module(), Length :: pos_integer()}.

%% @doc Create a new instance with a default page size of (1024 * 1024)
-spec new() -> State :: state().
new() ->
    new(?DEFAULT_PAGE_SIZE).

%% @doc Create a new instance with the given page size.  Will error if
%% page size is not a power of two.
-spec new(PageSize :: pos_integer()) -> State :: state().
new(PageSize) ->
    {
        memory_pager:new(PageSize),
        0
    }.

%% @doc Write data to memory at the given byte offset
-spec write(Offset :: pos_integer(), Data :: binary(), State :: state()) -> {ok, State :: state()}.
write(Offset, Data, {{Pager, PageSize}, Length}) ->
    PageNum = Offset div PageSize,
    PageCursor = (Offset - (PageNum * PageSize)),
    DataSize = byte_size(Data),
    NewLength = update_length(Offset, DataSize, Length),
    write_to_pages(
        0,
        PageCursor,
        PageNum,
        Data,
        DataSize,
        {{Pager, PageSize}, NewLength}
    ).

%% @private
write_to_pages(_, _, _, _, DataSize, State) when DataSize =:= 0 ->
    {ok, State};
write_to_pages(DataCursor, _, _, _, DataSize, State) when DataCursor >= DataSize ->
    {ok, State};
write_to_pages(DataCursor, PageCursor, PageNum, Data, DataSize, {{_, PageSize} = Mp, Len}) ->
    %% How much data is left
    DataBound = DataSize - DataCursor,
    %% What's the most we can write to the page to fill it
    %% based on the current position of the Page cursor?
    UpperBound = min(PageSize, PageCursor + DataBound),
    %% The amount of bytes we'll write
    RangeLen = UpperBound - PageCursor,

    %% Get the buffer for the given page
    PageBuffer =
        case memory_pager:get(PageNum, Mp) of
            {none, _} -> <<0:PageSize/unit:8>>;
            {ok, {_, B}, _} -> B
        end,
    %% Copy the data to the page buffer
    UpdatedBuffer = copy_binary(DataCursor, PageCursor, RangeLen, Data, PageBuffer),

    %% Write it to the page
    {ok, Mp1} = memory_pager:set(PageNum, UpdatedBuffer, Mp),

    %% Keep going while there's still data to process
    write_to_pages(
        DataCursor + RangeLen,
        0,
        PageNum + 1,
        Data,
        DataSize,
        {Mp1, Len}
    ).

%% @doc Get the page buffer for the given page number
-spec get_page(PageNum :: pos_integer(), State :: state()) -> none | {ok, Data :: binary()}.
get_page(PageNum, {Pager, _}) ->
    case memory_pager:get(PageNum, Pager) of
        {none, _} -> none;
        {ok, {_, B}, _} -> {ok, B}
    end.

%% @doc Read the given number of bytes from the byte offset.  This may 'walk'
%% several 'pages' to gather the results.
-spec read(Offset :: pos_integer(), BytesToRead :: pos_integer(), State :: state()) ->
    {error, out_of_bounds} | {ok, Result :: binary(), State :: state()}.
read(Offset, BytesToRead, {{_, _}, Length}) when (Offset + BytesToRead) > Length ->
    {error, out_of_bounds};
read(Offset, BytesToRead, {{_, PageSize}, _} = State) ->
    PageNum = Offset div PageSize,
    PageCursor = (Offset - (PageNum * PageSize)),
    OutBuffer = <<0:BytesToRead/unit:8>>,
    read_from_pages(
        PageNum,
        PageCursor,
        0,
        BytesToRead,
        OutBuffer,
        State
    ).

%% @private
read_from_pages(
    _,
    _,
    OutCursor,
    BytesToRead,
    OutBuffer,
    State
) when OutCursor >= BytesToRead ->
    {ok, OutBuffer, State};
read_from_pages(
    PageNum,
    PageCursor,
    OutCursor,
    BytesToRead,
    OutBuffer,
    {{_, PageSize} = Mp, _} = State
) ->
    %% Calculate the bounds for both binaries
    BufferBounds = BytesToRead - OutCursor,
    PageBounds = PageSize - PageCursor,

    %% Minimal amount to work with right now based on the read/write bounds
    MinimalBound = min(PageBounds, BufferBounds),

    %% Get the buffer for the given page and copy stuff
    case memory_pager:get(PageNum, Mp) of
        {ok, {_, PageBuffer}, _} ->
            OutBuffer1 = copy_binary(
                PageCursor,
                OutCursor,
                MinimalBound,
                PageBuffer,
                OutBuffer
            ),
            read_from_pages(
                PageNum + 1,
                0,
                OutCursor + MinimalBound,
                BytesToRead,
                OutBuffer1,
                State
            );
        _ ->
            {none, State}
    end.

%% @doc Delete the number of bytes starting at the offset.
-spec del(Offset :: pos_integer(), BytesToDelete :: pos_integer(), State :: state()) ->
    {ok, State :: state()}.
del(Offset, BytesToDelete, {{_, _}, _Length} = State) ->
    {ok, {{Pager, PageSize}, L}} = write(Offset, <<0:BytesToDelete/unit:8>>, State),
    NewLen =
        case Offset + BytesToDelete > L of
            true -> Offset;
            _ -> L
        end,
    {ok, {{Pager, PageSize}, NewLen}}.

%% @doc return the amount of data stored (in bytes)
-spec len(State :: state()) -> {ok, Amount :: pos_integer(), State :: state()}.
len({_, _, Length} = State) ->
    {ok, Length, State}.

%% @private Calculate the total number of bytes written
update_length(Offset, DataSize, Length) ->
    Nl = Offset + DataSize,
    case Nl > Length of
        true -> Nl;
        _ -> Length
    end.

%% @private Copy a binary from src to dest. given the cursor positions, amount
%% of data to copy and the respective buffers.
copy_binary(ReadCursor, WriteCursor, BytesToProcess, ReadData, WriteData) ->
    %% Consume bytes up to 'ReadCursor', 'CopyThis' the bytes you want
    <<_:ReadCursor/binary, CopyThis:BytesToProcess/binary, _/binary>> = ReadData,

    %% Capture the 'WriteCursor' number of bytes from the start of the write buffer
    %% Ignore 'BytesToProcess' number of bytes from the middle
    %% Capture what's left if anything
    <<Head:WriteCursor/binary, _:BytesToProcess/binary, R/binary>> = WriteData,

    %% Make the buffer:
    %% Put in the head, the bytes we copied and what was left.
    <<Head/binary, CopyThis:BytesToProcess/binary, R/binary>>.
