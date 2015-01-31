-module(game_invitation).

%% API
-export([ new/0
        , challenger/1
        ]).

-record(invitation,
        { challenger
        }).


%%%===================================================================
%%% API
%%%===================================================================

new() ->
    #invitation{ challenger = self()
               }.

challenger(#invitation{challenger = C}) -> C.


%%%===================================================================
%%% Internal functions
%%%===================================================================
