-module(user_controller).
-behaviour(gen_server).

%% API
-export([ start_link/0
        ]).

%% Client API
-export([ get_room_list/1
        , create_room/2
        , say/3
        , accept_game_invitation/2
        , deny_game_invitation/2
        , invite_to_game/2
        ]).

%% Server API
-export([ message/2
        , game_invitation/3
        , game_invitation_accepted/3
        , game_invitation_denied/3
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
        { socket_controller
        , joined_rooms = []
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    SocketController = self(),
    gen_server:start_link(?MODULE, [SocketController], []).


%%%
%%% Client API
%%%

get_room_list(User) ->
    gen_server:call(User, room_list).

create_room(User, Name) ->
    gen_server:call(User, {create_room, Name}).

say(User, Room, Message) ->
    gen_server:cast(User, {say, Room, Message}).

accept_game_invitation(User, Invitation) ->
    gen_server:call(User, {accept_game_invitation, Invitation}).

deny_game_invitation(User, Invitation) ->
    gen_server:call(User, {deny_game_invitation, Invitation}).

invite_to_game(User, Opponent) ->
    gen_server:call(User, {invitate_to_game, Opponent}).


%%%
%%% Server API
%%%

message(User, Message) ->
    gen_server:cast(User, {message, Message}).

game_invitation(User, Invitation, Opponent) ->
    gen_server:cast(User, {game_invitation, Invitation, Opponent}).

game_invitation_accepted(User, Invitation, Opponent) ->
    gen_server:cast(User, {game_invitation_accepted, Invitation, Opponent}).

game_invitation_denied(User, Invitation, Opponent) ->
    gen_server:cast(User, {game_invitation_denied, Invitation, Opponent}).


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
init([SocketController]) ->
    {ok, #user{socket_controller = SocketController}}.

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
handle_call(room_list, _From, S = #user{joined_rooms = Rooms}) ->
    Reply = {ok, Rooms},
    {reply, Reply, S};

handle_call({create_room, Name}, _From, S) ->
    room_sup:start_room(Name),
    {reply, ok, S};

handle_call({accept_game_invitation, Invitation}, _From, S) ->
    game_sup:accept_invitation(Invitation),
    {reply, ok, S};

handle_call({deny_game_invitation, Invitation}, _From, S) ->
    Opponent = undefined, %% TODO: Is the opponent included in the invitation?
    user_controller:send_game_invitation_denied(Opponent, Invitation),
    {reply, ok, S};

handle_call({invite_to_game, Opponent}, _From, S) ->
    Invitation = undefined, %% TODO: What is included in an invitation?
    user_controller:invited_to_game(Opponent, Invitation),
    {reply, ok, S};

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
handle_cast({say, Room, Message}, State) ->
    ok = room:say(Room, Message),
    {noreply, State};

handle_cast({message, Message}, S = #user{socket_controller = SC}) ->
    socket_controller:send_message(SC, Message),
    {noreply, S};

handle_cast({game_invitation, Invitation, Opponent}, S) ->
    SC = S#user.socket_controller,
    socket_controller:send_game_invitation(SC, Invitation, Opponent),
    {noreply, S};

handle_cast({game_invitation_accepted, Invitation, Opponent}, S) ->
    SC = S#user.socket_controller,
    socket_controller:send_game_invitation_accepted(SC, Invitation, Opponent),
    {noreply, S};

handle_cast({game_invitation_denied, Invitation, Opponent}, S) ->
    SC = S#user.socket_controller,
    socket_controller:send_game_invitation_denied(SC, Invitation, Opponent),
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
