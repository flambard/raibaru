-module(shell_user_adapter).
-behaviour(user_adapter).

%% API
-export([ start/0
        , receive_message/1
        ]).

%% User Adapter API
-export([ user_controller/1
        , send_message/2
        , send_game_invitation/2
        , send_game_invitation_accepted/3
        , send_game_invitation_denied/2
        , send_game_started/5
        , send_move/3
        ]).


%%%===================================================================
%%% API
%%%===================================================================

start() ->
    raibaru_user_controller_sup:start_user_controller(?MODULE, self()).

user_controller(UserController) ->
    UserController.

receive_message(UserController) ->
    receive
        {user_controller, UserController, Message} -> Message
    after 0 ->
            ok
    end.


%%%
%%% User Adapter API
%%%

send_message(Pid, Message) ->
    Pid ! {user_controller, self(), {message, Message}},
    ok.

send_game_invitation(Pid, Invitation) ->
    Pid ! {user_controller, self(), {invitation, Invitation}},
    ok.

send_game_invitation_accepted(Pid, Invitation, Game) ->
    Pid ! {user_controller, self(),
           {invitation_accepted, Invitation, Game}},
    ok.

send_game_invitation_denied(Pid, Invitation) ->
    Pid ! {user_controller, self(), {invitation_denied, Invitation}},
    ok.

send_game_started(Pid, Game, GameSettings, Color, Why) ->
    Pid ! {user_controller, self(),
           {game_started, Game, GameSettings, Color, Why}},
    ok.

send_move(Pid, Game, Move) ->
    Pid ! {user_controller, self(), {move, Game, Move}},
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
