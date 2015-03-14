-module(game_invitation).

%% API
-export([ new/1
        , challenger/1
        , challenger_color/1
        , opponent_color/1
        ]).

-record(invitation,
        { tag
        , challenger
        , challenger_color
        , opponent_color
        }).


%%%===================================================================
%%% API
%%%===================================================================

new(Color) ->
    #invitation{ tag = make_ref()
               , challenger = self()
               , challenger_color = Color
               , opponent_color = other_color(Color)
               }.

challenger(#invitation{challenger = C}) -> C.

challenger_color(#invitation{challenger_color = C}) -> C.

opponent_color(#invitation{opponent_color = C}) -> C.


%%%===================================================================
%%% Internal functions
%%%===================================================================

other_color(black) -> white;
other_color(white) -> black.
