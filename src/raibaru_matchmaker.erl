-module(raibaru_matchmaker).

%% API
-export([ start/0
        , find_match/1
        ]).

-define(SERVER, raibaru_matchmaker).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    matchmaker:start_matchmaker(?SERVER, game_sup, game_settings).

find_match(UserController) ->
    matchmaker:find_match(?SERVER, UserController).


%%%===================================================================
%%% Internal functions
%%%===================================================================
