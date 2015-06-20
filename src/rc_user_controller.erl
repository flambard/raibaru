-module(rc_user_controller).
-behaviour(gen_server).

%% API
-export([ start_link/2
        ]).

%% Inbound API
-export([ recv_get_room_list/1
        , recv_create_room/2
        , recv_say/3
        , recv_find_match/1
        , recv_game_invitation_accept/2
        , recv_game_invitation_deny/2
        , recv_game_invitation/4
        , recv_move/3
        ]).

%% Outbound API
-export([ send_message/2
        , send_game_invitation/2
        , send_game_invitation_denied/2
        , send_game_started/5
        , send_move/3
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(user,
        { adapter
        , module
        , joined_rooms = []
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(AdapterModule, Adapter) ->
    gen_server:start_link(?MODULE, [AdapterModule, Adapter], []).


%%%
%%% Inbound API
%%%

recv_get_room_list(User) ->
    gen_server:call(User, get_room_list).

recv_create_room(User, Name) ->
    gen_server:call(User, {create_room, Name}).

recv_say(User, Room, Message) ->
    gen_server:cast(User, {recv_say, Room, Message}).

recv_find_match(User) ->
    gen_server:cast(User, recv_find_match).

recv_game_invitation_accept(User, Invitation) ->
    gen_server:cast(User, {recv_game_invitation_accept, Invitation}).

recv_game_invitation_deny(User, Invitation) ->
    gen_server:cast(User, {recv_game_invitation_deny, Invitation}).

recv_game_invitation(User, Opponent, GameSettings, Color) ->
    gen_server:call(User,
                    {recv_game_invitation, Opponent, GameSettings, Color}).

recv_move(User, Game, Move) ->
    gen_server:call(User, {recv_move, Game, Move}).


%%%
%%% Outbound API
%%%

send_message(User, Message) ->
    gen_server:cast(User, {send_message, Message}).

send_game_invitation(User, Invitation) ->
    gen_server:cast(User, {send_game_invitation, Invitation}).

send_game_invitation_denied(User, Invitation) ->
    gen_server:cast(User, {send_game_invitation_denied, Invitation}).

send_game_started(User, Game, GameSettings, Color, Why) ->
    gen_server:cast(User, {send_game_started, Game, GameSettings, Color, Why}).

send_move(User, Game, Move) ->
    gen_server:cast(User, {send_move, Game, Move}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module, Adapter]) ->
    link(Adapter),
    {ok, #user{adapter = Adapter, module = Module}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_room_list, _From, S = #user{joined_rooms = Rooms}) ->
    Reply = {ok, Rooms},
    {reply, Reply, S};

handle_call({create_room, Name}, _From, S) ->
    {ok, Room} = raibaru_room_sup:start_room(Name),
    {reply, {ok, Room}, S};

handle_call({recv_game_invitation, Opponent, GameSettings, Color}, _From, S) ->
    Invitation = rc_game_invitation:new(GameSettings, Color),
    ?MODULE:send_game_invitation(Opponent, Invitation),
    {reply, {ok, Invitation}, S};

handle_call({recv_move, Game, Move}, _From, S) ->
    Reply = rc_game:move(Game, Move),
    {reply, Reply, S};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({recv_say, Room, Message}, State) ->
    ok = rc_room:say(Room, Message),
    {noreply, State};

handle_cast(recv_find_match, State) ->
    raibaru_matchmaker:find_match(self()),
    {noreply, State};

handle_cast({recv_game_invitation_accept, Invitation}, S) ->
    ChallengerColor =
        case rc_game_invitation:challenger_color(Invitation) of
            nigiri -> random_color();
            Color  -> Color
        end,
    case ChallengerColor of
        black ->
            raibaru_game_sup:start_game(
              rc_game_invitation:challenger(Invitation),
              self(),
              rc_game_invitation:game_settings(Invitation),
              {game_invitation, Invitation});
        white ->
            raibaru_game_sup:start_game(
              self(),
              rc_game_invitation:challenger(Invitation),
              rc_game_invitation:game_settings(Invitation),
              {game_invitation, Invitation})
    end,
    {noreply, S};

handle_cast({recv_game_invitation_deny, Invitation}, S) ->
    Opponent = rc_game_invitation:challenger(Invitation),
    ?MODULE:send_game_invitation_denied(Opponent, Invitation),
    {noreply, S};

handle_cast({send_message, Message}, S = #user{module = M}) ->
    M:send_message(S#user.adapter, Message),
    {noreply, S};

handle_cast({send_game_invitation, Invitation}, S = #user{module = M}) ->
    M:send_game_invitation(S#user.adapter, Invitation),
    {noreply, S};

handle_cast({send_game_invitation_denied, Invitation}, S) ->
    M = S#user.module,
    M:send_game_invitation_denied(S#user.adapter, Invitation),
    {noreply, S};

handle_cast({send_game_started, Game, GameSettings, Color, Why}, S) ->
    monitor(process, Game),
    M = S#user.module,
    M:send_game_started(S#user.adapter, Game, GameSettings, Color, Why),
    {noreply, S};

handle_cast({send_move, Game, Move}, S) ->
    M = S#user.module,
    ok = M:send_move(S#user.adapter, Game, Move),
    {noreply, S};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, _Type, _Pid, _Info}, State) ->
    %% TODO: A game process died, clean up
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_color() ->
    case random:uniform(2) of
        1 -> black;
        2 -> white
    end.
