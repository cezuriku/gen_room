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
