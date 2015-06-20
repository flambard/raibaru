-module(rc_game).
-behaviour(gen_fsm).

%% API
-export([ start_link/4
        , move/2
        ]).

%% gen_fsm callbacks
-export([ init/1
        , state_name/2
        , awaiting_move/3
        , handle_event/3
        , handle_sync_event/4
        , handle_info/3
        , terminate/3
        , code_change/4
        ]).

-record(state,
        { settings
        , black
        , white
        , opponent_passed = false
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Player1, Player2, GameSettings, Why) ->
    gen_fsm:start_link(?MODULE, [Player1, Player2, GameSettings, Why], []).

move(Game, Move) ->
    gen_fsm:sync_send_event(Game, {move, Move}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Player1, Player2, Settings, Why]) ->
    monitor(process, Player1),
    monitor(process, Player2),
    user_controller:send_game_started(Player1, self(), Settings, black, Why),
    user_controller:send_game_started(Player2, self(), Settings, white, Why),
    {ok, awaiting_move, #state{ settings = Settings
                              , black = Player1
                              , white = Player2
                              }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
awaiting_move({move, resign}, {Pid, _Tag}, State) ->
    case State of
        #state{black = Pid} -> io:format("black resign\n");
        #state{white = Pid} -> io:format("white resign\n")
    end,
    {stop, normal, ok, State};

awaiting_move({move, pass}, {Pid, _Tag}, S) when S#state.opponent_passed ->
    case S of
        #state{black = Pid} -> io:format("black pass\n");
        #state{white = Pid} -> io:format("white pass\n")
    end,
    {stop, normal, ok, S};

awaiting_move({move, Move}, {Pid, _Tag}, S) ->
    %% TODO: Check if move is legal
    %% TODO: Record the move
    case S of
        #state{black = Pid} -> io:format("black ~p\n", [Move]);
        #state{white = Pid} -> io:format("white ~p\n", [Move])
    end,
    Opponent = case S of
                   #state{black = Pid} -> S#state.white;
                   #state{white = Pid} -> S#state.black
               end,
    ok = user_controller:send_move(Opponent, self(), Move),
    {reply, ok, awaiting_move, S#state{opponent_passed = Move =:= pass}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, _Type, _Pid, _Info}, _StateName, State) ->
    %% TODO: A user controller process died, mark player as away
    {noreply, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================







