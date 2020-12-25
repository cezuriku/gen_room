-module(gen_room_test).

-include_lib("mockgyver/include/mockgyver.hrl").

start_room_test() ->
    {ok, RoomPid} = gen_room:start_link(),
    ?_assert(is_pid(RoomPid)).

stop_room_test() ->
    {ok, RoomPid} = gen_room:start_link(),
    gen_room:stop(RoomPid).

gen_room_test_() ->
    ?WITH_MOCKED_SETUP(fun setup/0, fun cleanup/1).

setup() ->
    ?WHEN(dummy_module:handle_client_added(_, _, _) -> ok),
    ?WHEN(dummy_module:handle_client_removed(_, _) -> ok),
    ?WHEN(dummy_module:handle_packet(_, _, _, _) -> ok),
    {ok, RoomPid} = gen_room:start_link(),
    RoomPid.

cleanup(RoomPid) ->
    gen_room:stop(RoomPid).

gen_room_register_client_test(RoomPid) ->
    {ok, 0, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid, dummy_data).

gen_room_register_two_clients_test(RoomPid) ->
    {ok, 0, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid1, dummy_data1),
    {ok, 1, #{0 := dummy_data1}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid2, dummy_data2),
    ?WAS_CALLED(dummy_module:handle_client_added(dummy_pid1, 1, dummy_data2)).

gen_room_unregister_client_test(RoomPid) ->
    {ok, 0, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid1, dummy_data1),
    {ok, 1, #{0 := dummy_data1}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid2, dummy_data2),
    ok = gen_room:unregister_client(RoomPid, 0),
    ?WAS_CALLED(dummy_module:handle_client_removed(dummy_pid2, 0)).

gen_room_send_packet_test(RoomPid) ->
    {ok, 0, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid0, dummy_data),
    {ok, 1, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid1, dummy_data),

    ok = gen_room:send(RoomPid, 0, 1, dummy_data1),
    ?WAS_CALLED(dummy_module:handle_packet(dummy_pid1, 0, private, dummy_data1)).

gen_room_send_all_packet_test(RoomPid) ->
    {ok, 0, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid0, dummy_data),
    {ok, 1, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid1, dummy_data),
    {ok, 2, #{}} =
        gen_room:register_client(RoomPid, dummy_module, dummy_pid2, dummy_data),

    ok = gen_room:send_all(RoomPid, 0, dummy_data1),
    ?WAS_CALLED(dummy_module:handle_packet(dummy_pid1, 0, public, dummy_data1)),
    ?WAS_CALLED(dummy_module:handle_packet(dummy_pid2, 0, public, dummy_data1)).
