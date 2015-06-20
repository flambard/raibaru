-module(rc_gnugo_game_map).

%% API
-export([ new/0
        , add/4
        , delete_gnugo_ref/2
        , find_gnugo_ref/2
        , find_game_id/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================

new() -> [].

add(GameID, GnuGoRef, Color, Map) ->
    lists:keystore(GameID, 1, Map, {GameID, GnuGoRef, Color}).

delete_gnugo_ref(GnuGoRef, Map) ->
    lists:keydelete(GnuGoRef, 2, Map).

find_gnugo_ref(GameID, Map) ->
    lists:keyfind(GameID, 1, Map).

find_game_id(GnuGoRef, Map) ->
    lists:keyfind(GnuGoRef, 2, Map).


%%%===================================================================
%%% Internal functions
%%%===================================================================
