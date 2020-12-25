-module(gen_room).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_client/4,
    unregister_client/2,
    send/4,
    send_all/3,
    stop/1
]).

%% gen_server callback
-export([
    terminate/3,
    code_change/4,
    init/1,
    handle_cast/2,
    handle_call/3
]).

-export_types([client_id/0, client_data/0]).

-type client_id() :: integer().
-type client_data() :: term().

-type client() :: #{
    module := module(),
    pid := pid(),
    data := client_data()
}.

-type data() :: #{
    next_client_id := client_id(),
    clients := #{client_id() := client()}
}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec register_client(pid(), module(), pid(), term()) ->
    {ok, client_id(), OtherClients :: #{client_id() := client_data()}}.
register_client(GenRoomPid, ClientModule, ClientPid, ClientData) ->
    gen_server:call(GenRoomPid, {register_client, ClientModule, ClientPid, ClientData}).

-spec unregister_client(pid(), client_id()) -> ok.
unregister_client(Pid, ClientId) ->
    gen_server:cast(Pid, {unregister_client, ClientId}).

-spec send(pid(), client_id(), client_id(), term()) -> ok.
send(Pid, SenderId, TargetId, Packet) ->
    gen_server:cast(Pid, {send, SenderId, TargetId, Packet}).

-spec send_all(pid(), client_id(), term()) -> ok.
send_all(Pid, SenderId, Packet) ->
    gen_server:cast(Pid, {send_all, SenderId, Packet}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

-spec init([]) -> {ok, data()}.
init([]) ->
    {ok, #{next_client_id => 0, clients => #{}}}.

handle_cast({unregister_client, ClientId}, #{clients := Clients} = Data) ->
    NewClients = maps:without([ClientId], Clients),
    maps:fold(
        fun(_Id, #{module := CbModule, pid := Pid}, ok) ->
            ok = CbModule:handle_client_removed(Pid, ClientId)
        end,
        ok,
        NewClients
    ),
    {noreply, Data#{clients := NewClients}};
handle_cast({send, SenderId, TargetId, Packet}, #{clients := Clients} = Data) ->
    #{TargetId := #{module := CbModule, pid := TargetPid}} = Clients,
    ok = CbModule:handle_packet(TargetPid, SenderId, private, Packet),
    {noreply, Data};
handle_cast({send_all, SenderId, Packet}, #{clients := Clients} = Data) ->
    maps:fold(
        fun(_Id, #{module := CbModule, pid := Pid}, ok) ->
            ok = CbModule:handle_packet(Pid, SenderId, public, Packet)
        end,
        ok,
        maps:without([SenderId], Clients)
    ),
    {noreply, Data};
handle_cast(EventContent, Data) ->
    %% Ignore all other events
    io:format(
        "Unhandled event:~n~p~n",
        [
            #{
                event_type => cast,
                event_content => EventContent,
                data => Data
            }
        ]
    ),
    keep_state_and_data.

handle_call(
    {register_client, ClientModule, ClientPid, ClientData},
    _From,
    #{next_client_id := NextClientId, clients := Clients} = Data
) ->
    NewClient = #{
        module => ClientModule,
        pid => ClientPid,
        data => ClientData
    },
    maps:fold(
        fun(_Id, #{module := CbModule, pid := Pid}, ok) ->
            ok = CbModule:handle_client_added(Pid, NextClientId, ClientData)
        end,
        ok,
        Clients
    ),
    NewClients = Clients#{NextClientId => NewClient},
    CurrentClients = maps:fold(
        fun(Id, #{data := CData}, ClientsAcc) ->
            ClientsAcc#{Id => CData}
        end,
        #{},
        Clients
    ),
    {reply, {ok, NextClientId, CurrentClients}, Data#{
        next_client_id := NextClientId + 1,
        clients := NewClients
    }};
handle_call(EventContent, _From, Data) ->
    %% Reply with the current count
    io:format(
        "Unhandled event:~n~p~n",
        [
            #{
                event_type => call,
                event_content => EventContent,
                data => Data
            }
        ]
    ),
    {reply,
        {error,
            {unhandled_event, #{
                event => EventContent,
                data => Data
            }}},
        Data}.
