%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ocarina.dev.ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2013 by wanglihe <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(worker).

-behaviour(gen_server).

%% API
-export([start_link/5, start/1, cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("audio_core.hrl").
-include("video_core.hrl").
-include("media_desc.hrl").

-define(SERVER, ?MODULE).

-define(RES_RESERVE_PORT, 0).


-record(state, { play_play = none
               , play_payload_type
               , play_media_type
               , play_media_type_ext
               , play_commander
               , play_meta
               , play_next_step = none
               , play_barge_dc = none

               , dc_payload
               , dc_digit_collect
               , dc_commander
               , dc_meta
               , dc_para
               , dc_retry = 0

               , rec_record = none
               , rec_record_video = none
               , rec_commander
               , rec_meta

               , amr_mode = max
               , video_fps = 15

               , maxtime
               , amr_trans
               %, command_log = []
               , port = none
               , socket = none
               , local_ip
               , remote_addr
               , refnumber
               , type
               , ssrc
               , data_transfer
               , play_mode = none
               , dc_mode = none
               , rec_mode = none
               , confroom = []
               , audio_trans = none
               , video_trans = none
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
start_link(SSRCs, LocalIP, Ports, AmrTrans, MaxTime) ->
    gen_server:start_link(?MODULE, [SSRCs, LocalIP, Ports, AmrTrans, MaxTime], []).

start(RefNum) ->
    gen_server:start(?MODULE, [RefNum], []).

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
init([SSRCs, LocalIP, Ports, AmrTrans, MaxTime]) ->
    %%为加快worker_pool的速度，这里越简单越好，
    %%所以只用系统内的数据简单初始化
    process_flag(trap_exit, true),
    {ok, #state{ port = Ports
               , ssrc = SSRCs
               , local_ip = LocalIP
               , amr_trans = AmrTrans
               , maxtime = MaxTime}}.

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
handle_call({get, data_transfer}, _From, State) ->
    {DT, _, _, _, _} = State#state.audio_trans,
    {reply, {ok, DT}, State};

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
handle_cast( {Commander,{alloc_media, CallID, AllocPara, MetaData}}
           , #state{ local_ip = {IPV4, IPV6}
                   , port = Ports
                   , ssrc = SSRCs} = State) ->
    { {port_need, PortNeed}
      , { {addr, IPVer, IP}
        , AudioDesc
        , VideoDesc}} = AllocPara,
    LIP = case IPVer of
        inet -> IPV4;
        inet6 -> IPV6
    end,
    case alloc_resouce(IPVer, LIP, Ports) of
        {ok, SockDTs} ->
            set_worker_timeout(State#state.maxtime),
            case PortNeed of
                1 ->
                    [P] = Ports,
                    [SSRC] = SSRCs,
                    [{Sock, DT}] = SockDTs,
                    %%如需支持单视频，需要在这里加入判断
                    PortsRet = [port_return(AudioDesc, P)],
                    gen_server:cast(Commander, {alloc_media, CallID, {ok, IPVer, LIP, PortsRet}, MetaData}),
                    {noreply, State#state{ refnumber = CallID
                                         , remote_addr = IP
                                         , audio_trans = {DT, Sock, P, SSRC, AudioDesc}}};
                2 ->
                    [P1, P2] = Ports,
                    [SSRC1, SSRC2] = SSRCs,
                    [{Sock1, DT1}, {Sock2, DT2}] = SockDTs,
                    PortsRet = [ port_return(AudioDesc, P1)
                               , port_return(VideoDesc, P2)],
                    gen_server:cast(Commander, {alloc_media, CallID, {ok, IPVer, LIP, PortsRet}, MetaData}),
                    {noreply, State#state{ refnumber = CallID
                                         , remote_addr = IP
                                         , audio_trans = {DT1, Sock1, P1, SSRC1, AudioDesc}
                                         , video_trans = {DT2, Sock2, P2, SSRC2, VideoDesc}}}
            end;

        {error, eaddrinuse} ->
            gen_server:cast(Commander, {alloc_media, CallID, {error, ebadport}, {AllocPara, MetaData}}),
            {stop, {shutdown, ebadport}, State};

        {error, Reason} ->
            gen_server:cast(Commander, {alloc_media, CallID, {error, Reason}, MetaData}),
            info_manager:session(session_fail),
            {stop, {shutdown, Reason}, State}
    end;

handle_cast( {_Commander, {alloc_media_ack, _CallID, AckPara, MetaData}}, State) when AckPara =:= none ->
    #state{ remote_addr = Addr
          , audio_trans = AudioTrans
          , video_trans = VideoTrans
          } = State,
    set_data_transfer(Addr, AudioTrans),
    set_data_transfer(Addr, VideoTrans),
    {rcs, _, _, TimerStart} = MetaData,
    info_manager:time(time_confirm_expend, TimerStart),
    {noreply, State};

handle_cast( {_Commander, {alloc_media_ack, _CallID, AckPara, MetaData}}, State) ->
    #state{ audio_trans = OldAudioTrans
            , video_trans = OldVideoTrans
          } = State,
    {_, {{addr, _, Addr}, AudioDesc, VideoDesc}} = AckPara,
    AudioTrans = case OldAudioTrans of
                     none ->
                         none;
                     {DT1, Sock1, P1, SSRC1, _} ->
                         {DT1, Sock1, P1, SSRC1, AudioDesc}
                 end,
    VideoTrans = case OldVideoTrans of
                     none ->
                         none;
                     {DT2, Sock2, P2, SSRC2, _} ->
                         {DT2, Sock2, P2, SSRC2, VideoDesc}
                 end,
    set_data_transfer(Addr, AudioTrans),
    set_data_transfer(Addr, VideoTrans),
    {rcs, _, _, TimerStart} = MetaData,
    info_manager:time(time_confirm_expend, TimerStart),
    {noreply, State#state{ remote_addr = Addr
                           , audio_trans = AudioTrans
                           , video_trans = VideoTrans}};

handle_cast( {Commander, {play_media, CallID, PlayPara, MetaData}}, #state{play_mode = none} = State) ->
    {rcs, _RcsReq, TimerStart} = MetaData,
    case
        case PlayPara#media_ctrl.barge of
            false ->
                {ok, none};
            true ->
                Trans = State#state.audio_trans,
                DCPara = #digit_ctrl{barge = true},
                digit_collect:start_link(Trans, DCPara, client, os:timestamp())
        end
    of
        {ok, DCPid} ->
            AudioTrans = State#state.audio_trans,
            VideoTrans = State#state.video_trans,
            {_,_,_,_,AudioDesc} = AudioTrans,
            Codec = AudioDesc#audio_desc.codec,
            NFilenames = case need_trans_filenames(State#state.amr_trans, Codec, VideoTrans) of
                true ->
                    info_manager:info("amr trans is on and codec is ~p, filenames in play need trans", [Codec]),
                    {Modeset, _} = AudioDesc#audio_desc.amr_ext,
                    lists:map(fun(Filename) -> trans_file_name(Codec, Modeset, Filename) end, PlayPara#media_ctrl.filenames);
                false ->
                    PlayPara#media_ctrl.filenames
            end,
            case media_play:start_link({AudioTrans, VideoTrans}, PlayPara#media_ctrl{filenames = NFilenames}, client, TimerStart) of
                {ok, Pid} ->
                    info_manager:info("play running ~p",[Pid]),
                    {noreply, State#state{ play_play = Pid
                                         , play_mode = case DCPid of
                                                      none ->
                                                          play_playing;
                                                      _ ->
                                                          play_barging
                                                  end
                                         , play_next_step = return_play
                                         , play_barge_dc = DCPid
                                         , play_commander = Commander
                                         , play_meta = MetaData}};
                {error, {shutdown, Reason}} ->
                    info_manager:info("play error ~p",[Reason]),
                    #media_ctrl{ filenames = Filenames } = PlayPara,
                    info_manager:log_error("Media play error", io_lib:format("~p play error", [Filenames])),
                    %%gen_server:cast(self(), {none, {stop_media, CallID, none, none}}),
                    gen_server:cast(Commander, {play_media, CallID, {error, Reason}, MetaData}),
                    {noreply, State}
            end;
        {error, {shutdown, Reason}} ->
            info_manager:info("dc error ~p",[Reason]),
            info_manager:log_error("Digit collect error", io_lib:format("~p", [Reason])),
            worker:cast(Commander, {play_media, CallID, {error, Reason}, MetaData}),
            {noreply, State}
    end;

handle_cast({media_play_stop, Result}, #state{ play_mode = Play_Mode
                                             , play_next_step = return_play} = State) when Play_Mode =:= play_playing
                                                                                         ; Play_Mode =:= play_barging ->
    MetaData = State#state.play_meta,
    Commander = State#state.play_commander,
    CallID = State#state.refnumber,
    digit_collect:cast(State#state.play_barge_dc, stop),
    gen_server:cast(Commander, {play_media, CallID, Result, MetaData}),
    {noreply, State#state{ play_play = none
                         , play_mode = none
                         , play_barge_dc = none
                         , play_next_step = none}};

handle_cast({media_play_stop, _Result}, #state{ play_mode = Play_Mode
                                              , play_next_step = start_fdt
                                              , dc_mode = dc_running
                                              , rec_mode = none
                                              , dc_para = DCPara} = State) when Play_Mode =:= play_playing
                                                                              ; Play_Mode =:= play_barging ->
    DCPid = State#state.dc_digit_collect,
    StartFdtTimer = (DCPara#digit_ctrl.timer)#digit_timer.start_fdt_timer,
    case StartFdtTimer of
        true ->
            ok;
        false ->
            info_manager:info("starttimer=false, fdt is started when play over~n"),
            digit_collect:cast(DCPid, start_fdt)
    end,
    {noreply, State#state{ play_play = none
                         , play_mode = none
                         , play_next_step = none}};

handle_cast({media_play_stop, _Result}, #state{ play_mode = Play_Mode
                                              , rec_mode = rec_running
                                              , dc_mode = none} = State) when Play_Mode =:= play_playing
                                                                            ; Play_Mode =:= play_barging ->
    {noreply, State#state{ play_play = none
                         , play_mode = none}};

handle_cast( {Commander, {digit_collect, CallID, DCPara, MetaData}}, #state{ dc_mode = none
                                                                           , play_mode = Play_Mode} = State) ->
    info_manager:info("dc para is: ~p", [DCPara]),
    TimesRetry = DCPara#digit_ctrl.times,
    Trans = State#state.audio_trans,
    {rcs, _, StartTimer} = MetaData,
    case digit_collect:start_link(Trans, DCPara, client, StartTimer) of
        {ok, DCPid} ->
            info_manager:info("start dc ~p",[DCPid]),
            case DCPara#digit_ctrl.media of
                none ->
                    {noreply, State#state{ dc_mode = dc_running
                                         , dc_digit_collect = DCPid
                                         , dc_commander = Commander
                                         , dc_retry = TimesRetry - 1
                                         , dc_para = DCPara
                                         , dc_meta = MetaData}};
                PlayPara when Play_Mode =:= none ->
                    AudioTrans = State#state.audio_trans,
                    VideoTrans = State#state.video_trans,
                    {_,_,_,_,AudioDesc} = AudioTrans,
                    Codec = AudioDesc#audio_desc.codec,
                    Barge = DCPara#digit_ctrl.barge,
                    NFilenames = case need_trans_filenames(State#state.amr_trans, Codec, VideoTrans) of
                        true ->
                            info_manager:info("amr trans is on and codec is ~p, filenames in aupc need trans", [Codec]),
                            {Modeset, _} = AudioDesc#audio_desc.amr_ext,
                            lists:map(fun(Filename) -> trans_file_name(Codec, Modeset, Filename)
                                              end, PlayPara#media_ctrl.filenames);
                        false ->
                            PlayPara#media_ctrl.filenames
                    end,
                    case media_play:start_link({AudioTrans, VideoTrans}, PlayPara#media_ctrl{filenames = NFilenames}, client, os:timestamp()) of
                        {ok, Pid} ->
                            info_manager:info("digit play running ~p",[Pid]),
                            {noreply, State#state{ play_play = Pid
                                                 , play_mode = case Barge of
                                                                   false ->
                                                                       play_playing;
                                                                   true ->
                                                                       play_barging
                                                               end
                                                 , play_next_step = start_fdt
                                                 , dc_mode = dc_running
                                                 , dc_digit_collect = DCPid
                                                 , dc_commander = Commander
                                                 , dc_retry = TimesRetry - 1
                                                 , dc_para = DCPara
                                                 , dc_meta = MetaData}};
                        {error, {shutdown, Reason}} ->
                            info_manager:info("play error ~p",[Reason]),
                            #media_ctrl{ filenames = Filenames } = PlayPara,
                            info_manager:log_error("Media play error", io_lib:format("~p play error", [Filenames])),
                            %%gen_server:cast(self(), {none, {stop_media, CallID, none, none}}),
                            gen_server:cast(Commander, {digit_collect, CallID, {error, Reason}, MetaData}),
                            {noreply, State}
                    end;
                _ ->
                    info_manager:log_error("Digit collect error", "play_digit after play"),
                    worker:cast(Commander, {digit_collect, CallID, {error, none}, MetaData})
            end;
        {error, {shutdown, Reason}} ->
            info_manager:info("dc error ~p",[Reason]),
            info_manager:log_error("Digit collect error", io_lib:format("~p", [Reason])),
            worker:cast(Commander, {digit_collect, CallID, {error, Reason}, MetaData}),
            {noreply, State}
    end;

handle_cast(digit_collect_barge, #state{play_mode = play_barging} = State) ->
    media_play:cast(State#state.play_play, stop),
    {noreply, State#state{ play_mode = play_playing}};
handle_cast(digit_collect_barge, #state{play_mode = none} = State) ->
    {noreply, State};

handle_cast({digit_collect_stop, {ok, stopped, _}}, State) ->
    {noreply, State};
handle_cast({digit_collect_stop, Result}, #state{ play_mode = Play_Mode
                                                , play_next_step = start_fdt
                                                , dc_mode = dc_running} = State) when Play_Mode =:= play_playing
                                                                                    ; Play_Mode =:= play_barging ->
    %%当barge=fasle，收号结束时停止放音(这里要不要停放音，有待讨论)
    media_play:cast(State#state.play_play, stop),
    NState = State#state{ play_play = none
                        , play_mode = none
                        , play_next_step = none},
    handle_cast({digit_collect_stop, Result}, NState);

handle_cast({digit_collect_stop, {ok, Reason, _} = Result}, #state{ dc_mode = dc_running
                                                                  , dc_retry = TimesRetry} = State)
                                                                when TimesRetry =:= 0
                                                                   ; Reason =:= match ->
    MetaData = State#state.dc_meta,
    Commander = State#state.dc_commander,
    CallID = State#state.refnumber,
    gen_server:cast(Commander, {digit_collect, CallID, Result, MetaData}),
    {noreply, State#state{ dc_digit_collect = none
                         , dc_mode = none}};
handle_cast({digit_collect_stop, _Result}, #state{dc_mode = dc_running} = State) ->
    MetaData = State#state.dc_meta,
    Commander = State#state.dc_commander,
    CallID = State#state.refnumber,

    #digit_ctrl{times = TimesRetry} = State#state.dc_para,
    NDCPara = State#state.dc_para#digit_ctrl{times = TimesRetry - 1},
    handle_cast({Commander, {digit_collect, CallID, NDCPara, MetaData}}, State#state{dc_mode = none});

handle_cast({Commander, {record_media, CallID, #record_ctrl{type = video}=RecPara, MetaData}}, #state{rec_mode = none}=State) ->
    [AudioDest, VideoDest] = string:tokens(RecPara#record_ctrl.file_name, "|"),
    [AudioFormat|VideoFormat] = RecPara#record_ctrl.format,
    VideoRecPara = RecPara#record_ctrl{file_name = VideoDest, format = VideoFormat},
    info_manager:info("video rec para is: ~p", [VideoRecPara]),
    Trans = State#state.video_trans,
    {rcs, _, StartTimer} = MetaData,
    case media_record:start_link(Trans, VideoRecPara, client, StartTimer) of
        {ok, RecPid} ->
            AudioRecPara = RecPara#record_ctrl{type = audio, file_name = AudioDest, format = [AudioFormat]},
            handle_cast({Commander, {record_media, CallID, AudioRecPara, MetaData}}, State#state{rec_record_video = RecPid});
        {error, Reason} ->
            info_manager:info("video rec error ~p",[Reason]),
            info_manager:log_error("record video media error", io_lib:format("~p", [Reason])),
            worker:cast(Commander, {record_media, CallID, {error, Reason}, MetaData}),
            {noreply, State}
    end;
handle_cast( {Commander, {record_media, CallID, #record_ctrl{type = audio} = RecPara, MetaData}}, #state{ rec_mode = none
                                                                                           , play_mode = Play_Mode} = State) ->
    info_manager:info("audio rec para is: ~p", [RecPara]),
    Trans = State#state.audio_trans,
    {rcs, _, StartTimer} = MetaData,
    case media_record:start_link(Trans, RecPara, client, StartTimer) of
        {ok, RecPid} ->
            info_manager:info("start rec: audio ~p video ~p",[RecPid, State#state.rec_record_video]),
            case RecPara#record_ctrl.media of
                none ->
                    {noreply, State#state{ rec_mode = rec_running
                                         , rec_record = RecPid
                                         , rec_commander = Commander
                                         , rec_meta = MetaData}};
                PlayPara when Play_Mode =:= none ->
                    AudioTrans = State#state.audio_trans,
                    VideoTrans = State#state.video_trans,
                    Barge = RecPara#record_ctrl.barge,
                    case media_play:start_link({AudioTrans, VideoTrans}, PlayPara, client, os:timestamp()) of
                        {ok, Pid} ->
                            info_manager:info("rec play running ~p",[Pid]),
                            {noreply, State#state{ play_play = Pid
                                                 , play_mode = case Barge of
                                                                   false ->
                                                                       play_playing;
                                                                   true ->
                                                                       play_barging
                                                               end
                                                 , rec_mode = rec_running
                                                 , rec_record = RecPid
                                                 , rec_commander = Commander
                                                 , rec_meta = MetaData}};
                        {error, {shutdown, Reason}} ->
                            info_manager:info("play error ~p",[Reason]),
                            #media_ctrl{ filenames = Filenames } = PlayPara,
                            info_manager:log_error("Media play error", io_lib:format("~p play error", [Filenames])),
                            %%gen_server:cast(self(), {none, {stop_media, CallID, none, none}}),
                            gen_server:cast(Commander, {record_media, CallID, {error, Reason}, MetaData}),
                            {noreply, State}
                    end;
                _ ->
                    info_manager:log_error("Media record error", "play_record after play"),
                    worker:cast(Commander, {record_media, CallID, {error, none}, MetaData})
            end;
        {error, Reason} ->
            info_manager:info("audio rec error ~p",[Reason]),
            info_manager:log_error("record audio media error", io_lib:format("~p", [Reason])),
            worker:cast(Commander, {record_media, CallID, {error, Reason}, MetaData}),
            {noreply, State}
    end;

handle_cast(record_media_barge, #state{play_mode = play_barging, rec_mode = rec_running} = State) ->
    media_play:cast(State#state.play_play, stop),
    {noreply, State#state{play_mode = play_playing}};
handle_cast(record_media_barge, #state{play_mode = none} = State) ->
    {noreply, State};

handle_cast({record_media_stop, Result}, #state{ play_mode = Play_Mode
                                               , play_next_step = none
                                               , rec_mode = rec_running} = State) when Play_Mode =:= play_playing
                                                                                     ; Play_Mode =:= play_barging ->
    media_play:cast(State#state.play_play, stop),
    NState = State#state{ play_play = none
                        , play_mode = none},
    handle_cast({record_media_stop, Result}, NState);

handle_cast({record_media_stop, Result}, #state{rec_mode = rec_running} = State) ->
    case Result of
        {ok, termkey, _} ->
            media_record:cast(State#state.rec_record_video, stop);
        _ ->
            ok
    end,
    MetaData = State#state.rec_meta,
    Commander = State#state.rec_commander,
    CallID = State#state.refnumber,
    gen_server:cast(Commander, {record_media, CallID, Result, MetaData}),
    {noreply, State#state{ rec_record = none
                         , rec_mode = none}};

handle_cast( {_Commander, {stop_pa, CallID, _, _MetaData}} ,#state{ play_mode = Play_Mode
                                                                  , play_next_step = return_play} = State)
                                                               when Play_Mode =:= play_playing
                                                                  ; Play_Mode =:= play_barging ->
    info_manager:info("stop pa ~p",[CallID]),
    report_req_ivr_succ(Play_Mode),
    digit_collect:cast(State#state.play_barge_dc, stop),
    media_play:cast(State#state.play_play, stop),
    {noreply, State#state{ play_play = none
                         , play_barge_dc = none
                         , play_mode = none
                         , play_next_step = none}};

handle_cast( {_Commander, {stop_pc, _CallID, _, _MetaData}} = Command, #state{ dc_mode = dc_running
                                                                             , play_next_step = start_fdt} = State) ->
    media_play:cast(State#state.play_play, stop),
    NState = State#state{ play_play = none
                        , play_mode = none
                        , play_next_step = none},
    handle_cast(Command, NState);
handle_cast( {_Commander, {stop_pc, CallID, _, _MetaData}}, #state{dc_mode = dc_running} = State) ->
    info_manager:info("stop pc ~p",[CallID]),
    report_req_ivr_succ(dc_running),
    digit_collect:cast(State#state.dc_digit_collect, stop),
    {noreply, State#state{ dc_digit_collect = none
                         , dc_mode = none}};

handle_cast( {_Commander, {stop_pr, CallID, _, _MetaData}}, #state{ rec_mode = rec_running
                                                                  , play_mode = none} = State) ->
    info_manager:info("stop pr ~p",[CallID]),
    report_req_ivr_succ(rec_running),
    media_record:cast(State#state.rec_record, stop),
    media_record:cast(State#state.rec_record_video, stop),
    {noreply, State#state{ rec_record = none
                         , rec_mode = none}};
handle_cast( {_Commander, {stop_pr, _CallID, _, _MetaData}} = Command, #state{ rec_mode = rec_running
                                                                             , play_next_step = none} = State) ->
    media_play:cast(State#state.play_play, stop),
    NState = State#state{ play_play = none
                        , play_mode = none},
    handle_cast(Command, NState);

handle_cast(release, #state{ play_mode = Play_Mode
                           , play_next_step = return_play} = State) ->
    report_req_ivr_succ(Play_Mode),
    NState = State#state{ play_mode = none
                        , play_next_step = none},
    handle_cast(release, NState);
handle_cast(release, #state{ dc_mode = DC_Mode
                           , rec_mode = Rec_Mode} = State) ->
    report_req_ivr_succ(DC_Mode),
    report_req_ivr_succ(Rec_Mode),
    {stop, normal, State};

handle_cast(Msg, State) ->
    info_manager:info("worker unexpect msg ~p",[Msg]),
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
    {stop, {shutdown, Reason}, State};
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
terminate(_Reason, State) ->
    lists:foreach(fun({_, Socket, _, _, _}) ->
                        catch gen_udp:close(Socket);
                     (_) ->
                          ok
                  end,[State#state.audio_trans, State#state.video_trans]).

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

set_worker_timeout(infinity) ->
    ok;
set_worker_timeout(Maxtime) when is_integer(Maxtime) ->
    erlang:start_timer(Maxtime*1000, self(), worker_timeout).

alloc_data_transfer(IPVer, _IP, Port) ->
    case catch gen_udp:open(Port, [binary,{active,false}, IPVer]) of
        {ok, Socket} ->
            case data_transfer:start_link() of
                {ok, Pid} ->
                    {ok, Socket, Pid};
                {error, Reason} ->
                    gen_udp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};

        {'EXIT', Reason} ->
            {error, Reason};
        _ ->
            {error, unknown}
    end.
alloc_resouce(_, _, []) ->
    {ok, []};
alloc_resouce(IPVer, IP, [Port|Ports]) ->
    case alloc_data_transfer(IPVer, IP, Port) of
        {ok, Socket, Pid} ->
            case alloc_resouce(IPVer, IP, Ports) of
                {ok, Result} ->
                    {ok, [{Socket, Pid}| Result]};
                {error, Reason} ->
                    gen_udp:close(Socket),
                    exit(Pid, kill),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

set_data_transfer(_Addr, none) ->
    ok;
set_data_transfer(_Addr, {_, _, _, _, none})->
    ok;
set_data_transfer(Addr, {DT, Socket, _, SSRC, Desc})->
    Port = case Desc of
        #audio_desc{port = P} ->
            P;
        #video_desc{port = P} ->
            P
    end,
    data_transfer:set(DT, {rtp, {Socket, {Addr, Port}, SSRC, Desc}}).

port_return(none, _) -> ?RES_RESERVE_PORT;
port_return(_, Port) -> Port.

need_trans_filenames(false, _, _) ->
    false;
need_trans_filenames(true, Codec, none) when Codec =:= amrnb; Codec =:= amrwb ->
    true;
need_trans_filenames(true, _, _) ->
    false.

trans_file_name(Codec, Mode, Filename) ->
    Title = case string:rchr(Filename, $.) of
        0 ->
            Filename;
        Index ->
            string:substr(Filename, 1, Index-1)
    end,
    case Codec of
        amrnb ->
            string:concat(string:concat(string:concat(Title, "_"), integer_to_list(Mode)), ".amr");
        amrwb ->
            string:concat(string:concat(string:concat(Title, "_wb_"), integer_to_list(Mode)), ".amr")
    end.

report_req_ivr_succ(none) ->
    ok;
report_req_ivr_succ(Mode) when Mode =:= play_playing; Mode =:= play_barging ->
    info_manager:req(req_play_succ);
report_req_ivr_succ(Mode) when Mode =:= dc_running ->
    info_manager:req(req_pc_succ);
report_req_ivr_succ(Mode) when Mode =:= rec_running ->
    info_manager:req(req_pr_succ).
