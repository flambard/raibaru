-module(raibaru_core_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {one_for_one, 1000, 3600},

    Lobby = {raibaru_lobby,
             {raibaru_lobby, start_link, []},
             permanent,
             2000,
             worker,
             [raibaru_lobby]},

    RoomSup = {room_sup,
               {room_sup, start_link, []},
               permanent,
               2000,
               supervisor,
               [room_sup]},

    GameSup = {game_sup,
               {game_sup, start_link, []},
               permanent,
               2000,
               supervisor,
               [game_sup]},

    UserControllerSup = {user_controller_sup,
                         {user_controller_sup, start_link, []},
                         permanent,
                         2000,
                         supervisor,
                         [user_controller_sup]},


    SocketListenerSup = {socket_listener_sup,
                         {socket_listener_sup, start_link, []},
                         permanent,
                         2000,
                         supervisor,
                         [socket_listener_sup]},

    {ok, {SupFlags,
          [ Lobby
          , RoomSup
          , GameSup
          , UserControllerSup
          , SocketListenerSup
          ]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
