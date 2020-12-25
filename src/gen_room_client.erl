-module(gen_room_client).

-callback handle_client_added(
    Pid :: pid(),
    Id :: gen_room:client_id(),
    Data :: gen_room:client_data()
) -> ok.

-callback handle_client_removed(
    Pid :: pid(),
    Id :: gen_room:client_id()
) -> ok.

-callback handle_packet(
    Pid :: pid(),
    From :: gen_room:client_id(),
    Scope :: public | private,
    Packet :: term()
) -> ok.
