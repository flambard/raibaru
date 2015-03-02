-module(user_controller_sup).
-behaviour(supervisor).

%% API
-export([ start_link/0
        , start_user_controller/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_user_controller(Module, Pid) ->
    supervisor:start_child(?SERVER, [Module, Pid]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 1000, 3600},
    ChildSpec = {user_controller,
                 {user_controller, start_link, []},
                 temporary,
                 2000,
                 worker,
                 [user_controller]},
    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
