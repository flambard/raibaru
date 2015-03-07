-module(gnugo_user_adapter).
-behaviour(gen_server).

%% API
-export([ start_link/0
        , user_controller/1
        , send_message/2
        , send_game_invitation/2
        , send_game_invitation_accepted/3
        , send_game_invitation_denied/3
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

-record(state,
        { user_controller
        , map
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

user_controller(Server) ->
    gen_server:call(Server, user_controller).

send_message(_Server, _Message) ->
    %% Messages are ignored.
    ok.

send_game_invitation(Server, Invitation) ->
    gen_server:cast(Server, {game_invitation, Invitation}).

send_game_invitation_accepted(_Server, _Invitation, _Opponent) ->
    %% Ignored, GNU Go does not send game invitations.
    ok.

send_game_invitation_denied(_Server, _Invitation, _Opponent) ->
    %% Ignored, GNU Go does not send game invitations.
    ok.

send_move(Server, Game, Move) ->
    gen_server:cast(Server, {move, Game, Move}).


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
init([]) ->
    {ok, Pid} = user_controller_sup:start_user_controller(?MODULE, self()),
    {ok, #state{ user_controller = Pid
               , map = gnugo_game_map:new()
               }}.


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
handle_call(user_controller, _From, State = #state{user_controller = UC}) ->
    {reply, UC, State};

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
handle_cast({game_invitation, Invitation}, State = #state{map = Map}) ->
    UC = State#state.user_controller,
    {ok, GameID} = user_controller:accept_game_invitation(UC, Invitation),
    Color = game_invitation:opponent_color(Invitation),
    {ok, Ref} = gnugo:new(),
    NewMap = gnugo_game_map:add(GameID, Ref, Color, Map),
    {noreply, State#state{map = NewMap}};

handle_cast({move, GameID, Move}, State = #state{map = Map}) ->
    {GameID, Ref, Color} = gnugo_game_map:find_gnugo_ref(GameID, Map),
    ok = gnugo:play(Ref, other_color(Color), Move),
    ok = gnugo:genmove_async(Ref, Color),
    {noreply, State};

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
handle_info({Ref, {data, {eol, Line}}}, State = #state{map = Map}) ->
    %% Received asynchronous reply from GNU Go.
    UC = State#state.user_controller,
    {ok, Move} = gnugo:receive_reply(Ref, Line),
    {GameID, Ref, _Color} = gnugo_game_map:find_game_id(Ref, Map),
    ok = user_controller:move(UC, GameID, Move),
    {noreply, State};

handle_info({Ref, {exit_status, _Status}}, State = #state{map = Map}) ->
    NewMap = gnugo_game_map:delete_gnugo_ref(Ref, Map),
    {noreply, State#state{map = NewMap}};

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

other_color(black) -> white;
other_color(white) -> black.
