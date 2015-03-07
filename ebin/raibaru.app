{application, raibaru,
 [{description, ""},
  {vsn, "0.0.1"},
  {modules, [ raibaru_app
            , raibaru_sup
            , lobby
            , room_sup
            , room
            , game
            , game_invitation
            , game_sup
            , user_controller_sup
            , user_controller
            , socket_listener_sup
            , socket_listener
            , socket_user_adapter
            , gnugo
            , gnugo_user_adapter
            , gnugo_game_map
            , gtp
            , vertex
            ]},
  {registered, [ raibaru_sup
               , lobby
               , room_sup
               , game_sup
               , socket_listener_sup
               , user_controller_sup
               ]},
  {mod, {raibaru_app, []}},
  {applications, [ kernel
                 , stdlib
                 , sasl
                 , matchmaker
                 ]},
  {env, []}
 ]}.
