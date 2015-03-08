-module(user_adapter).

%% API
-export([]).


-callback user_controller(Adapter :: pid()) ->
    UserController :: pid().

-callback send_message(Adapter :: pid(), Message :: term()) ->
    ok.

-callback send_game_invitation(Adapter :: pid(), Invitation :: term()) ->
    ok.

-callback send_game_invitation_accepted(Adapter :: pid(),
                                        Invitation :: term(),
                                        Game :: term()) ->
    ok.

-callback send_game_invitation_denied(Adapter :: pid(), Invitation :: term()) ->
    ok.

-callback send_move(Adapter :: pid(), Game :: term(), Move :: term()) ->
    ok.


%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% Internal functions
%%%===================================================================
