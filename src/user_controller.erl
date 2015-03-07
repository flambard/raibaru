-module(user_controller).
-behaviour(gen_server).

%% API
-export([ start_link/2
        ]).

%% Client API
-export([ get_room_list/1
        , create_room/2
        , say/3
        , accept_game_invitation/2
        , deny_game_invitation/2
        , invite_to_game/2
        , move/3
        ]).

%% Server API
-export([ message/2
        , game_invitation/2
        , game_invitation_accepted/3
        , game_invitation_denied/2
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
    gen_server:call(User, {invite_to_game, Opponent}).

move(User, Game, Move) ->
    gen_server:call(User, {move, Game, Move}).


%%%
%%% Server API
%%%

message(User, Message) ->
    gen_server:cast(User, {message, Message}).

game_invitation(User, Invitation) ->
    gen_server:cast(User, {game_invitation, Invitation}).

game_invitation_accepted(User, Invitation, Game) ->
    gen_server:cast(User, {game_invitation_accepted, Invitation, Game}).

game_invitation_denied(User, Invitation) ->
    gen_server:cast(User, {game_invitation_denied, Invitation}).


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
handle_call(room_list, _From, S = #user{joined_rooms = Rooms}) ->
    Reply = {ok, Rooms},
    {reply, Reply, S};

handle_call({create_room, Name}, _From, S) ->
    room_sup:start_room(Name),
    {reply, ok, S};

handle_call({accept_game_invitation, Invitation}, _From, S) ->
    {ok, Game} = game_sup:accept_invitation(Invitation),
    Opponent = game_invitation:challenger(Invitation),
    user_controller:game_invitation_accepted(Opponent, Invitation, Game),
    {reply, {ok, Game}, S};

handle_call({deny_game_invitation, Invitation}, _From, S) ->
    Opponent = game_invitation:challenger(Invitation),
    user_controller:game_invitation_denied(Opponent, Invitation),
    {reply, ok, S};

handle_call({invite_to_game, Opponent}, _From, S) ->
    Invitation = game_invitation:new(),
    user_controller:game_invitation(Opponent, Invitation),
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

handle_cast({message, Message}, S = #user{module = M}) ->
    M:send_message(S#user.adapter, Message),
    {noreply, S};

handle_cast({game_invitation, Invitation}, S = #user{module = M}) ->
    M:send_game_invitation(S#user.adapter, Invitation),
    {noreply, S};

handle_cast({game_invitation_accepted, Invitation, Game}, S) ->
    M = S#user.module,
    M:send_game_invitation_accepted(S#user.adapter, Invitation, Game),
    {noreply, S};

handle_cast({game_invitation_denied, Invitation}, S) ->
    M = S#user.module,
    M:send_game_invitation_denied(S#user.adapter, Invitation),
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
