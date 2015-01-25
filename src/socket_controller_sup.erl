-module(socket_controller_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0
        , start_socket_controller/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_socket_controller(Socket) ->
    supervisor:start_child(?SERVER, [Socket]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 1000, 3600},
    ChildSpec = {socket_controller,
                 {socket_controller, start_link, []},
                 temporary,
                 2000,
                 worker,
                 [socket_controller]},
    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
