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

    RoomSup = {raibaru_room_sup,
               {raibaru_room_sup, start_link, []},
               permanent,
               2000,
               supervisor,
               [raibaru_room_sup]},

    GameSup = {raibaru_game_sup,
               {raibaru_game_sup, start_link, []},
               permanent,
               2000,
               supervisor,
               [raibaru_game_sup]},

    UserControllerSup = {user_controller_sup,
                         {user_controller_sup, start_link, []},
                         permanent,
                         2000,
                         supervisor,
                         [user_controller_sup]},


    SocketListenerSup = {raibaru_socket_listener_sup,
                         {raibaru_socket_listener_sup, start_link, []},
                         permanent,
                         2000,
                         supervisor,
                         [raibaru_socket_listener_sup]},

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
