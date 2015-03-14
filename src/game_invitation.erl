-module(game_invitation).

%% API
-export([ new/0
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

new() ->
    #invitation{ tag = make_ref()
               , challenger = self()
               , challenger_color = white
               , opponent_color = black
               }.

challenger(#invitation{challenger = C}) -> C.

challenger_color(#invitation{challenger_color = C}) -> C.

opponent_color(#invitation{opponent_color = C}) -> C.


%%%===================================================================
%%% Internal functions
%%%===================================================================
