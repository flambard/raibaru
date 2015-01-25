{application, raibaru,
 [{description, ""},
  {vsn, "0.0.1"},
  {modules, [ raibaru_app
            , raibaru_sup
            , lobby
            , room_sup
            , room
            , user_controller
            , socket_listener_sup
            , socket_listener
            , socket_controller_sup
            , socket_controller
            ]},
  {registered, [ raibaru_sup
               , lobby
               , room_sup
               , socket_listener_sup
               , socket_controller_sup
               ]},
  {mod, {raibaru_app, []}},
  {applications, [ kernel
                 , stdlib
                 , sasl
                 ]},
  {env, []}
 ]}.
