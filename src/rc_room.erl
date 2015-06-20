-module(rc_room).
-behaviour(gen_server).

%% API
-export([ start_link/1
        , get_users_list/1
        , join/1
        , leave/1
        , say/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(room,
        { name
        , users = []
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

get_users_list(Room) ->
    gen_server:call(Room, get_users_list).

join(Room) ->
    gen_server:call(Room, join).

leave(Room) ->
    gen_server:call(Room, leave).

say(Room, Message) ->
    gen_server:cast(Room, {message, Message}).

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
init([Name]) ->
    ok = raibaru_lobby:register_room(self(), Name),
    {ok, #room{name = Name}}.

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
handle_call(get_users_list, _From, S = #room{users = Users}) ->
    {reply, {ok, Users}, S};

handle_call(join, From, S = #room{users = Users}) ->
    Monitor = monitor(process, From),
    {reply, ok, S#room{users = [{From, Monitor} | Users]}};

handle_call(leave, From, S = #room{users = Users}) ->
    {From, Monitor} = lists:keyfind(From, 1, Users),
    demonitor(Monitor),
    {reply, ok, S#room{users = lists:keydelete(Users, 1, From)}};

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
handle_cast({message, Message}, S = #room{users = Users}) ->
    lists:foreach(fun({User, _Monitor}) ->
                          rc_user_controller:send_message(User, Message)
                  end,
                  Users),
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
handle_info({'DOWN', _Monitor, _Type, Pid, _Info}, S = #room{users = Users}) ->
    {noreply, S#room{users = lists:keydelete(Pid, 1, Users)}};

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
