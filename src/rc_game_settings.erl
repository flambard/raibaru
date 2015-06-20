-module(rc_game_settings).

%% API
-export([ new/0
        , boardsize/1
        , komi/1
        , handicap/1
        , ruleset/1
        ]).

-record(game_settings,
        { boardsize = 19
        , komi = 6.5
        , handicap = 0
        , ruleset = japanese
        }).


%%%===================================================================
%%% API
%%%===================================================================

new() ->
    #game_settings{}.

boardsize(#game_settings{boardsize = S}) -> S.

komi(#game_settings{komi = K}) -> K.

handicap(#game_settings{handicap = H}) -> H.

ruleset(#game_settings{ruleset = R}) -> R.


%%%===================================================================
%%% Internal functions
%%%===================================================================
