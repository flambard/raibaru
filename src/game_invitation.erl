-module(game_invitation).

%% API
-export([ new/2
        , game_settings/1
        , challenger/1
        , challenger_color/1
        , opponent_color/1
        ]).

-record(invitation,
        { tag
        , settings
        , challenger
        , challenger_color
        , opponent_color
        }).


%%%===================================================================
%%% API
%%%===================================================================

new(GameSettings, Color) ->
    #invitation{ tag = make_ref()
               , settings = GameSettings
               , challenger = self()
               , challenger_color = Color
               , opponent_color = other_color(Color)
               }.

game_settings(#invitation{settings = S}) -> S.

challenger(#invitation{challenger = C}) -> C.

challenger_color(#invitation{challenger_color = C}) -> C.

opponent_color(#invitation{opponent_color = C}) -> C.


%%%===================================================================
%%% Internal functions
%%%===================================================================

other_color(nigiri) -> nigiri;
other_color(black)  -> white;
other_color(white)  -> black.
