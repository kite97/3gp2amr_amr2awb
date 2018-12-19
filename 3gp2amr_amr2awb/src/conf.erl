%%%-------------------------------------------------------------------
%%% File    : conf.erl
%%% Author  : <lijinyu@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created :  26 Feb 2014 by <lijinyu@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(conf).

-behaviour(gen_server).

%% API
-export([start_link/1, cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("conference.hrl").
-include("audio_core.hrl").
-include("media_desc.hrl").

-define(SERVER, ?MODULE).

-record(state, { conf_id
               , mode = none
               , commander_play
               , meta_data_play
               , play_play = none
               , commander_digit_collect
               , meta_data_digit_collect
               , dc_digit_collect = none
               , dc_para
               , dc_retry = 0
               , commander_record
               , meta_data_record
               , record_record = none
               , data_transfer
               , data_transfer_record
               , mixer
               , channels
               , members = []
               , enable_dc = []}).

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
start_link(ConfRoomID) ->
    gen_server:start_link(?MODULE, [ConfRoomID], []).

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
init([ConfRoomID]) ->
    process_flag(trap_exit, true),
    case {data_transfer:start_link(), data_transfer:start_link()} of
        {{ok, DT1}, {ok, DT2}} ->
            AudioDesc = #audio_desc{codec = ?AUDIO_PCM8K16},
            data_transfer:set(DT1, {audio_desc, AudioDesc}),
            data_transfer:set(DT2, {audio_desc, AudioDesc}),
            {ok, #state{ conf_id = ConfRoomID
                       , data_transfer = DT1
                       , data_transfer_record = DT2
                       , channels = lists:seq(1, ?MAXMIXCHANNEL)}};
        {{error, Reason}, _} ->
            error_logger:info_msg("conf start error ~p~n", [Reason]),
            exit(Reason);
        {_, {error, Reason}} ->
            error_logger:info_msg("conf start error ~p~n", [Reason]),
            exit(Reason)
    end.

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
handle_cast({Commander, {alloc_conf, {_, RoomID}, _AllocPara, MetaData}}, #state{ conf_id = RoomID
                                                                                , data_transfer = DT
                                                                                , data_transfer_record = DTRecord} = State) ->
    case mixer:start_link() of
        {ok, Pid} ->
            case State#state.channels of
                [] ->
                    gen_server:cast(Commander, {alloc_conf, RoomID, {error, emax}, MetaData}),
                    info_manager:session(session_fail),
                    {stop, emax, State};
                [Channel1, Channel2|Channels] ->
                    case {catch gen_server:call(Pid, {join, Channel1, DT}), catch gen_server:call(Pid, {join, Channel2, DTRecord})} of
                        {{ok}, {ok}} ->
                            put(DT, Channel1),
                            put(DTRecord, Channel2),
                            gen_server:cast(Commander, {alloc_conf, RoomID, {ok}, MetaData}),
                            {noreply, State#state{ mixer = Pid
                                                 , channels = Channels}};
                        Reason ->
                            gen_server:cast(Commander, {alloc_conf, RoomID, {error, Reason}, MetaData}),
                            info_manager:session(session_fail),
                            {stop, Reason, State}
                    end
            end;
        {error, Reason} ->
            gen_server:cast(Commander, {alloc_conf, RoomID, {error, Reason}, MetaData}),
            info_manager:session(session_fail),
            {stop, Reason, State}
    end;
handle_cast({Commander, {join_conf, RoomID, JoinPara, MetaData}}, #state{conf_id = RoomID} = State) ->
    {member, MemberID} = JoinPara,
    case gen_server:call(worker_pool, {find, {Commander, MemberID}}) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, {get, data_transfer}) of
                {ok, DT} ->
                    case State#state.channels of
                        [] ->
                            gen_server:cast(Commander, {join_conf, RoomID, {error, ejoinmax}, MetaData}),
                            {noreply, State};
                        [Channel|Channels] ->
                            case catch gen_server:call(State#state.mixer, {join, Channel, DT}) of
                                {ok} ->
                                    put(DT, Channel),
                                    gen_server:cast(Commander, {join_conf, RoomID, {ok}, MetaData}),
                                    {noreply, State#state{channels = Channels}};
                                _ ->
                                    gen_server:cast(Commander, {join_conf, RoomID, {error, emixer}, MetaData}),
                                    {noreply, State}
                            end
                    end;
                _ ->
                    gen_server:cast(Commander, {join_conf, RoomID, {error, eworker}, MetaData}),
                    {noreply, State}
            end;
        {error, Reason} ->
            gen_server:cast(Commander, {join_conf, RoomID, {error, Reason}, MetaData}),
            {noreply, State}
    end;
handle_cast({Commander, {unjoin_conf, RoomID, UnJoinPara, MetaData}}, #state{conf_id = RoomID} = State) ->
    {member, MemberID} = UnJoinPara,
    case gen_server:call(worker_pool, {find, {Commander, MemberID}}) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, {get, data_transfer}) of
                {ok, DT} ->
                    case erase(DT) of
                        undefined ->
                            gen_server:cast(Commander, {unjoin_conf, RoomID, {error, exist}, MetaData}),
                            {noreply, State};
                        Channel ->
                            gen_server:call(State#state.mixer, {unjoin, Channel}),
                            gen_server:cast(Commander, {unjoin_conf, RoomID, {ok}, MetaData}),
                            Channels = State#state.channels,
                            {noreply, State#state{channels = [Channel|Channels]}}
                    end;
                _ ->
                    gen_server:cast(Commander, {unjoin_conf, RoomID, {error, eworker}, MetaData}),
                    {noreply, State}
            end;
        {error, Reason} ->
            gen_server:cast(Commander, {unjoin_conf, RoomID, {error, Reason}, MetaData}),
            {noreply, State}
    end;
handle_cast({Commander, {conf_play, RoomID, ConfPlayPara, MetaData}}, #state{ conf_id = RoomID
                                                                            , data_transfer = DT
                                                                            , mixer = Mixer} = State) ->
    {rcs, _, TimerStart} = MetaData,
    AudioDesc = #audio_desc{codec = ?AUDIO_PCM8K16},
    AudioTrans = {DT, fake, fake, fake, AudioDesc},
    case
        case ConfPlayPara#media_ctrl.barge of
            false ->
                {ok, none};
            true ->
                DCPara = #digit_ctrl{barge = true},
                digit_collect:start_link(AudioTrans, DCPara, mixer, TimerStart)
        end
    of
        {ok, DCPid} ->
            case media_play:start_link({AudioTrans, none}, ConfPlayPara, Mixer, TimerStart) of
                {ok, Pid} ->
                    info_manager:info("conf play running ~p",[Pid]),
                    {noreply, State#state{ play_play = Pid
                                         , mode = case DCPid of
                                                      none ->
                                                          confpa_playing;
                                                      _ ->
                                                          confpa_barging
                                                  end
                                         , meta_data_play = MetaData
                                         , commander_play = Commander }};
                {error, {shutdown, Reason}} ->
                    info_manager:info("conf play error ~p",[Reason]),
                    #media_ctrl{ filenames = Filenames } = ConfPlayPara,
                    info_manager:log_error("Media play error", io_lib:format("~p play error", [Filenames])),
                    gen_server:cast(Commander, {conf_play, RoomID, {error, Reason}, MetaData}),
                    {noreply, State}
            end;
        {error, {shutdown, Reason}} ->
            info_manager:info("conf dc error ~p",[Reason]),
            info_manager:log_error("Digit collect error", io_lib:format("~p", [Reason])),
            gen_server:cast(Commander, {conf_play, RoomID, {error, Reason}, MetaData}),
            {noreply, State}
    end;

handle_cast({media_play_stop, Result}, #state{conf_id = RoomID} = State) ->
    MetaData = State#state.meta_data_play,
    Commander = State#state.commander_play,
    gen_server:cast(Commander, {conf_play, RoomID, Result, MetaData}),
    {noreply, State#state{play_play = none}};

handle_cast({_Commander, {stop_confpa_media, RoomID, _, _MetaData}}, State) ->
    info_manager:info("stop confpa media ~p", [RoomID]),
    media_play:cast(State#state.play_play, stop),
    {noreply, State#state{ play_play = none}};

handle_cast({Commander, {conf_digit_collect, RoomID, DCPara, MetaData}}, #state{ conf_id = RoomID
                                                                               , data_transfer = DT} = State) ->
    TimesRetry = DCPara#digit_ctrl.times,
    {rcs, _, TimerStart} = MetaData,
    Trans = {DT, fake, fake, fake, #audio_desc{codec = ?AUDIO_PCM8K16}},
    case digit_collect:start_link(Trans, DCPara, mixer, TimerStart) of
         {ok, Pid} ->
             info_manager:info("conf start digit collect ~p",[Pid]),
             case DCPara#digit_ctrl.media of
                 none ->
                    {noreply, State#state{ dc_digit_collect = Pid
                                         , dc_para = DCPara
                                         , dc_retry = TimesRetry - 1
                                         , meta_data_digit_collect = MetaData
                                         , commander_digit_collect = Commander}};
                 PlayPara ->
                     case media_play:start_link({Trans, none}, PlayPara, mixer, os:timestamp()) of
                         {ok, PlayPid} ->
                             info_manager:info("conf digit play running ~p",[PlayPid]),
                             {noreply, State#state{ dc_digit_collect = Pid
                                                  , dc_para = DCPara
                                                  , dc_retry = TimesRetry - 1
                                                  , meta_data_digit_collect = MetaData
                                                  , commander_digit_collect = Commander
                                                  , play_play = PlayPid}};
                         {error, {shutdown, Reason}} ->
                             info_manager:info("conf digit play error ~p",[Reason]),
                             #media_ctrl{ filenames = Filenames } = PlayPara,
                             info_manager:log_error("Media play error", io_lib:format("~p play error", [Filenames])),
                             gen_server:cast(Commander, {conf_digit_collect, RoomID, {error, Reason}, MetaData}),
                             {noreply, State}
                     end
             end;
         {error, {shutdown, Reason}} ->
             info_manager:info("conf digit collect error ~p",[Reason]),
             gen_server:cast(Commander, {conf_digit_collect, RoomID, {error, Reason}, MetaData}),
             {noreply, State}
    end;

handle_cast({digit_collect_stop, Result}, #state{ conf_id = RoomID
                                                , dc_retry = 0} = State) ->
    MetaData = State#state.meta_data_digit_collect,
    Commander = State#state.commander_digit_collect,
    gen_server:cast(Commander, {conf_digit_collect, RoomID, Result, MetaData}),
    {noreply, State#state{dc_digit_collect = none}};

handle_cast({digit_collect_stop, _Result}, #state{conf_id = RoomID} = State) ->
    MetaData = State#state.meta_data_digit_collect,
    Commander = State#state.commander_digit_collect,
    #digit_ctrl{times = TimesRetry} = State#state.dc_para,
    NDCPara = State#state.dc_para#digit_ctrl{times = TimesRetry - 1},
    handle_cast({Commander, {conf_digit_collect, RoomID, NDCPara, MetaData}}, State);

handle_cast(digit_collect_barge, #state{mode = confpa_barging} = State) ->
    media_play:cast(State#state.play_play, stop),
    {noreply, State#state{ play_play = none
                         , mode = none}};

handle_cast({_Commander, {stop_conf_dc, RoomID, _, _MetaData}}, State) ->
    info_manager:info("stop conf digit collect ~p", [RoomID]),
    digit_collect:cast(State#state.dc_digit_collect, stop),
    {noreply, State#state{ dc_digit_collect = none
                         , dc_retry = 0 }};

handle_cast({Commander, {conf_record_media, RoomID, RecPara, MetaData}}, #state{ conf_id = RoomID
                                                                               , data_transfer = DT
                                                                               , data_transfer_record = DTRecord} = State) ->
    {rcs, _, TimerStart} = MetaData,
    TransRecord = {DTRecord, fake, fake, fake, #audio_desc{codec = ?AUDIO_PCM8K16}},
    case media_record:start_link(TransRecord, RecPara, mixer, TimerStart) of
         {ok, Pid} ->
             info_manager:info("conf start record ~p",[Pid]),
             case RecPara#record_ctrl.media of
                 none ->
                    {noreply, State#state{ record_record = Pid
                                         , meta_data_record = MetaData
                                         , commander_record = Commander}};
                 PlayPara ->
                     Trans = {DT, fake, fake, fake, #audio_desc{codec = ?AUDIO_PCM8K16}},
                     case media_play:start_link({Trans, none}, PlayPara, mixer, os:timestamp()) of
                         {ok, PlayPid} ->
                             info_manager:info("conf record play running ~p",[PlayPid]),
                             {noreply, State#state{ record_record = Pid
                                                  , meta_data_record = MetaData
                                                  , commander_record = Commander
                                                  , play_play = PlayPid}};
                         {error, {shutdown, Reason}} ->
                             info_manager:info("conf record error ~p",[Reason]),
                             #media_ctrl{ filenames = Filenames } = PlayPara,
                             info_manager:log_error("Media play error", io_lib:format("~p play error", [Filenames])),
                             gen_server:cast(Commander, {conf_record_media, RoomID, {error, Reason}, MetaData}),
                             {noreply, State}
                     end
             end;
         {error, {shutdown, Reason}} ->
             info_manager:info("conf record error ~p",[Reason]),
             gen_server:cast(Commander, {conf_record_media, RoomID, {error, Reason}, MetaData}),
             {noreply, State}
    end;

handle_cast({record_media_stop, Result}, #state{conf_id = RoomID} = State) ->
    MetaData = State#state.meta_data_record,
    Commander = State#state.commander_record,
    gen_server:cast(Commander, {conf_record_media, RoomID, Result, MetaData}),
    {noreply, State#state{record_record = none}};

handle_cast({_Commander, {stop_confpr_media, RoomID, _, _MetaData}}, State) ->
    info_manager:info("stop confpr media ~p", [RoomID]),
    media_record:cast(State#state.record_record, stop),
    {noreply, State#state{record_record = none}};

handle_cast(release, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    %info_manager:info("conf unexpect msg ~p",[Msg]),
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
handle_info({timeout, _, Reason}, State) ->
    {stop, Reason, State};
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
