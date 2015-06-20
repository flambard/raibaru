-module(raibaru_socket_listener_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0
        , start_listener/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_listener() ->
    supervisor:start_child(?SERVER, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Port = 8888,
    SocketOptions = [{packet, line}, {reuseaddr, true}],
    {ok, ListenSocket} = gen_tcp:listen(Port, SocketOptions),
    spawn_link(fun initial_listeners/0),

    SupFlags = {simple_one_for_one, 1000, 3600},
    ChildSpec = {rc_socket_listener,
                 {rc_socket_listener, start_link, [ListenSocket]},
                 temporary,
                 2000,
                 worker,
                 [rc_socket_listener]},

    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

initial_listeners() ->
    [start_listener() || _ <- lists:seq(1, 3)],
    ok.
