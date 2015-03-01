-module(gnugo).
-behaviour(gen_server).

%% API
-export([ start_link/0
        , protocol_version/1
        , name/1
        , version/1
        , known_command/2
        , list_commands/1
        , quit/1
        , boardsize/2
        , clear_board/1
        , komi/2
        , fixed_handicap/2
        , place_free_handicap/2
        , set_free_handicap/2
        , play/3
        , genmove/2
        , undo/1
        , time_settings/4
        , time_left/4
        , final_score/1
        , final_status_list/2
        , loadsgf/3
        , reg_genmove/2
        , showboard/1
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
        { port
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

protocol_version(Server) ->
    gen_server:call(Server, {gtp, protocol_version}).

name(Server) ->
    gen_server:call(Server, {gtp, name}).

version(Server) ->
    gen_server:call(Server, {gtp, version}).

known_command(Server, Command) ->
    gen_server:call(Server, {gtp, {known_command, Command}}).

list_commands(Server) ->
    gen_server:call(Server, {gtp, list_commands}).

quit(Server) ->
    gen_server:call(Server, {gtp, quit}).

boardsize(Server, Size) ->
    gen_server:call(Server, {gtp, {boardsize, Size}}).

clear_board(Server) ->
    gen_server:call(Server, {gtp, clear_board}).

komi(Server, NewKomi) ->
    gen_server:call(Server, {gtp, {komi, NewKomi}}).

fixed_handicap(Server, NumberOfStones) ->
    gen_server:call(Server, {gtp, {fixed_handicap, NumberOfStones}}).

place_free_handicap(Server, NumberOfStones) ->
    gen_server:call(Server, {gtp, {place_free_handicap, NumberOfStones}}).

set_free_handicap(Server, Vertices) ->
    gen_server:call(Server, {gtp, {set_free_handicap, Vertices}}).

play(Server, Color, Move) ->
    gen_server:call(Server, {gtp, {play, Color, Move}}).

genmove(Server, Color) ->
    gen_server:call(Server, {gtp, {genmove, Color}}, infinity).

undo(Server) ->
    gen_server:call(Server, {gtp, undo}).

time_settings(Server, MainTime, ByoYomiTime, ByoYomiStones) ->
    gen_server:call(Server, {gtp, {time_settings,
                                    MainTime,
                                    ByoYomiTime,
                                    ByoYomiStones}}).

time_left(Server, Color, Time, Stones) ->
    gen_server:call(Server, {gtp, {time_left, Color, Time, Stones}}).

final_score(Server) ->
    gen_server:call(Server, {gtp, final_score}, infinity).

final_status_list(Server, Status) ->
    gen_server:call(Server, {gtp, {final_status_list, Status}}, infinity).

loadsgf(Server, FileName, MoveNumber) ->
    gen_server:call(Server, {gtp, {loadsgf, FileName, MoveNumber}}).

reg_genmove(Server, Color) ->
    gen_server:call(Server, {gtp, {reg_genmove, Color}}, infinity).

showboard(Server) ->
    gen_server:call(Server, {gtp, showboard}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case os:find_executable("gnugo") of
        false    -> {stop, could_not_find_gnugo_executable};
        FilePath ->
            Args = ["--mode", "gtp"],
            Port = open_port({spawn_executable, FilePath},
                             [ {args, Args}
                             , {line, 128}
                             , exit_status
                             , hide
                             ]),
            {ok, #state{port = Port}}
    end.


handle_call({gtp, quit}, _From, State = #state{port = Port}) ->
    port_command(Port, gtp:command(quit)),
    {stop, normal, State};
handle_call({gtp, Command}, _From, State = #state{port = Port}) ->
    port_command(Port, gtp:command(Command)),
    CommandReply = receive_reply(Port),
    Reply = gtp:parse_command_reply(Command, CommandReply),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({Port, {exit_status, Status}}, State = #state{port = Port}) ->
    {stop, {port_exited, Status}, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(normal, #state{port = Port}) ->
    port_close(Port);
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

receive_reply(Port) ->
    receive_reply(Port, []).

receive_reply(Port, Acc) ->
    receive
        {Port, {data, {eol, []}}}   -> string:join(lists:reverse(Acc), "\n");
        {Port, {data, {eol, Line}}} -> receive_reply(Port, [Line | Acc])
    end.
