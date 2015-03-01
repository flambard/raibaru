-module(vertex).

%% API
-export([ from_string/1
        , to_iolist/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

from_string([Letter | Digits]) ->
    {list_to_atom(string:to_lower([Letter])), list_to_integer(Digits)}.

to_iolist({Letter, Digit}) ->
    io_lib:format("~p~B", [Letter, Digit]).


%%%===================================================================
%%% Internal functions
%%%===================================================================
