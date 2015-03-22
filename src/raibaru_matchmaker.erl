-module(raibaru_matchmaker).
-behaviour(matchmaker).

%% API
-export([ start/0
        , find_match/1
        ]).

%% matchmaker callbacks
-export([ start_game/2
        ]).

-define(SERVER, raibaru_matchmaker).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    matchmaker:start_matchmaker(?SERVER, ?MODULE).

find_match(UserController) ->
    matchmaker:find_match(?SERVER, UserController).


%%%===================================================================
%%% matchmaker callbacks
%%%===================================================================

start_game(Player1, Player2) ->
    game_sup:start_game(Player1, Player2, game_settings:new(), matchmaker),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
