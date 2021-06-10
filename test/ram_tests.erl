-module(ram_tests).

-include_lib("eunit/include/eunit.hrl").

pow2_test() ->
    ?assertError({badarg, not_power_of_two}, ra_storage_memory:new(3)),
    ok.

small_test() ->
    S1 = ra_storage_memory:new(4),

    %% Page 0: Write 1 byte at index 1
    {ok, S2} = ra_storage_memory:write(1, <<2>>, S1),
    {ok, <<0, 2, 0, 0>>} = ra_storage_memory:get_page(0, S2),

    %% Page 1: Write 2 bytes starting at index 5
    {ok, S3} = ra_storage_memory:write(5, <<3, 3>>, S2),
    {ok, <<0, 3, 3, 0>>} = ra_storage_memory:get_page(1, S3),

    %% Handle <<>> data
    {ok, S3} = ra_storage_memory:write(6, <<>>, S3),

    %% Page: 2 Fill the page
    {ok, S4} = ra_storage_memory:write(8, <<4, 4, 4, 4>>, S3),
    {ok, <<4, 4, 4, 4>>} = ra_storage_memory:get_page(2, S4),

    %% Page 1: Overwrite the data
    {ok, S5} = ra_storage_memory:write(5, <<6, 6>>, S4),
    {ok, <<0, 6, 6, 0>>} = ra_storage_memory:get_page(1, S5),

    %% Check length
    {_, _, 12} = S5,
    ok.

overlapping_data_test() ->
    S1 = ra_storage_memory:new(4),

    %% 0: [0,0,0,1] 1: [2,2,2,2] 2: [3,3,0,0]
    {ok, S2} = ra_storage_memory:write(3, <<1, 2, 2, 2, 2, 3, 3>>, S1),
    {ok, <<0, 0, 0, 1>>} = ra_storage_memory:get_page(0, S2),
    {ok, <<2, 2, 2, 2>>} = ra_storage_memory:get_page(1, S2),
    {ok, <<3, 3, 0, 0>>} = ra_storage_memory:get_page(2, S2),

    %% Write a byte to index 10000 (page 2500)
    {ok, S3} = ra_storage_memory:write(10000, <<7>>, S2),
    {ok, <<7, 0, 0, 0>>} = ra_storage_memory:get_page(2500, S3),
    {_, _, 10001} = S3,
    ok.

read_data_test() ->
    S1 = ra_storage_memory:new(4),
    {ok, S2} = ra_storage_memory:write(0, <<3, 3>>, S1),
    {ok, <<3, 3>>, S2} = ra_storage_memory:read(0, 2, S2),
    {ok, <<3>>, S2} = ra_storage_memory:read(0, 1, S2),

    {ok, S3} = ra_storage_memory:write(2, <<1, 2, 2, 2, 2, 3, 3, 4>>, S2),
    {ok, <<1, 2, 2, 2, 2, 3, 3, 4>>, S3} = ra_storage_memory:read(2, 8, S3),
    ok.
