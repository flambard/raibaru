-module(raibaru_matchmaker).
-behaviour(matchmaker).

%% API
-export([ start/0
        , find_match/1
        ]).

%% matchmaker callbacks
-export([ start_game/2
        ]).

-record(player_info,
        {}).

-define(SERVER, raibaru_matchmaker).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    matchmaker:start_matchmaker(?SERVER, ?MODULE).

find_match(UserController) ->
    Info = #player_info{},
    matchmaker:find_match(?SERVER, UserController, Info).


%%%===================================================================
%%% matchmaker callbacks
%%%===================================================================

start_game({Player1, _Info1}, {Player2, _Info2}) ->
    %% TODO: Use player info to determine boardsize, handicap, komi, ruleset.
    Settings = rc_game_settings:new(),
    raibaru_game_sup:start_game(Player1, Player2, Settings, matchmaker),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
