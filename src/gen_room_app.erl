%%%-------------------------------------------------------------------
%% @doc gen_room public API
%% @end
%%%-------------------------------------------------------------------

-module(gen_room_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gen_room_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
