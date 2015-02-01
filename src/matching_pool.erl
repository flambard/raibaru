-module(matching_pool).

%% API
-export([ new/0
        , match_player/2
        , remove_player/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

new() -> [].

match_player(Pool, Player) ->
    case find_match(Player, Pool) of
        no_match          -> {no_match, add(Player, Pool)};
        {match, Opponent} -> {match, Opponent, delete(Opponent, Pool)}
    end.

remove_player(Pool, Player) ->
    delete(Pool, Player).


%%%===================================================================
%%% Internal functions
%%%===================================================================

add(Player, Pool) ->
    [Player | Pool].

delete(Player, Pool) ->
    lists:delete(Player, Pool).

find_match(_Player, []) ->
    no_match;
find_match(_Player, [P | _]) ->
    {match, P}.
