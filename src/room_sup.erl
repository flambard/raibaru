-module(room_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0
        , start_room/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_room() ->
    supervisor:start_child(?SERVER, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 1000, 3600},
    ChildSpec = {room,
                 {room, start_link, []},
                 temporary,
                 2000,
                 worker,
                 [room]},

    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
