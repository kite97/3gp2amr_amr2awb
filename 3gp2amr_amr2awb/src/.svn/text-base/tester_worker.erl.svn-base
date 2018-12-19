%%%-------------------------------------------------------------------
%%% @author lijinyu <lijinyu@ocarina.dev.ebupt.com>
%%% @copyright (C) 2014, lijinyu
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2014 by lijinyu <lijinyu@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(tester_worker).

-behaviour(gen_server).

%% API
-export([start_link/6, cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(RING_PATH, "../voice/local/").

-record(state, { socket = none
               , ssrc
               , data_transfer
               , mode = none
               , type
               , send
               , play_file
               , send_file
               , local_ip
               , grand
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(SSRC, Socket, Type, Param, LocalIP, Grand) ->
    gen_server:start_link(?MODULE, [SSRC, Socket, Type, Param, LocalIP, Grand], []).

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
init([SSRC, Socket, Type, Param, LocalIP, Grand]) ->
    process_flag(trap_exit, true),
    {NType, Send, PF, SF} = case Type of
        play ->
            {play, false, Param, none};
        play_send ->
            {PFile, SFile} = Param,
            {play, true, PFile, SFile};
        Type when Type =:= dtmf;
                  Type =:= rec ->
            {Type, true, none, Param};
        Type when Type =:= aupc;
                  Type =:= aupr ->
            {PFile, SFile} = Param,
            {Type, true, PFile, SFile}
    end,
    IP = lists:map(fun(S) ->
             list_to_integer(S) end,
             string:tokens(LocalIP, ".")),
    {ok, #state{ socket = Socket
               , ssrc = SSRC
               , type = NType
               , send = Send
               , play_file = PF
               , send_file = SF
               , local_ip = IP
               , grand = Grand}}.

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
handle_call(_Request, _From, State) ->
    {reply, {ok}, State}.

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
handle_cast({alloc, 0, RefNum, AddrPort}, #state{ local_ip = LocalIP
                                                     , grand = Grand
                                                     , send = Send
                                                     , send_file = SF
                                                     , ssrc = SSRC } = State) ->
    Socket = State#state.socket,
    {ok, Port} = inet:port(Socket),
    case data_transfer:start_link(Socket) of
        {ok, DataTransfer} ->
            [IP1,IP2,IP3,IP4] = LocalIP,
            Ack = icp:encode({service, {ack, {IP4, IP3, IP2, IP1, Port}, 0}}, RefNum),
            ocarina_tester:cast(Grand, {alloc, {ok, Ack}}),
            data_transfer:set(DataTransfer, {rtp, [{client, AddrPort}
                                          , {payloadtype, 0}
                                          , {dc_mode, {1, 100}}]}),
            Data = case Send of
                true ->
                    load_data(SF);
                false ->
                    <<>>
            end,
            data_transfer:start_service(DataTransfer, { send
                                                      , {Data, SSRC}
                                                      , {0, 1}}),
            {noreply, State#state{data_transfer = DataTransfer
                                , mode = ack}};
        {error, Reason} ->
            ocarina_tester:cast(Grand, {alloc, RefNum, {error, Reason}}),
            {stop, Reason, State}
    end;

handle_cast({service, Result, RefNum}, #state{ type = Type
                                             , play_file = PF
                                             , grand = Grand } = State) when Result =:= 0 ->
    Info = case Type of
        play ->  %% barge=false
            icp:encode({service, {play, {0, PF}, 2}}, RefNum);
        dtmf ->
            icp:encode({service, {dtmf, {0, 16}, 2}}, RefNum);
        rec ->
            icp:encode({service, {rec, {10, io_lib:format("record-~p.wav", [RefNum])}, 2}}, RefNum);
        aupc ->  %% barge=true
            icp:encode({service, {aupc, {{0, PF}, {1, 16}}, 2}}, RefNum);
        aupr ->  %% barge=false
            icp:encode({service, {play, {0, PF}, 3}}, RefNum)
    end,
    ocarina_tester:cast(Grand, {ack, {ok, Info}}),
    {noreply, State#state{mode = info}};

handle_cast({service, _Result, RefNum}, #state{ type = Type
                                              , data_transfer = DataTransfer
                                              , grand = Grand } = State) when Type =:= play;
                                                                              Type =:= dtmf;
                                                                              Type =:= rec ->
    Release = icp:encode(release, RefNum),
    ocarina_tester:cast(Grand, {info, {ok, Release}}),
    pulse_server:remove_player(DataTransfer),
    {stop, normal, State};
    %{noreply, State#state{mode = release}};

handle_cast({service, Result, RefNum}, #state{ type = Type
                                             , data_transfer = DataTransfer
                                             , grand = Grand } = State) ->
    case {Type, Result} of
        {aupc, 16#101} ->
            {noreply, State};
        {aupr, 16#101} ->
            Info = icp:encode({service, {rec, {10, io_lib:format("aupr-~p.wav", [RefNum])}, 2}}, RefNum),
            ocarina_tester:cast(Grand, {info, {ok, Info}}),
            {noreply, State};
        _ ->
            Release = icp:encode(release, RefNum),
            ocarina_tester:cast(Grand, {info, {ok, Release}}),
            pulse_server:remove_player(DataTransfer),
            {stop, normal, State}
            %{noreply, State#state{mode = release}};
    end;

handle_cast(_Msg, State) ->
    %%info_manager:info("worker unexpect msg ~p",[Msg]),
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

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_data(Filename) ->
    case file:read_file(?RING_PATH++Filename) of
        {ok, RawData} ->
            RawData;
        {error, Reason} ->
            info_manager:info("read file error: ~p", [Reason]),
            exit(stop)
    end.

