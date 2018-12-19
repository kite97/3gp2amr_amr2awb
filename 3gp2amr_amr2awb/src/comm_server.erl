%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  4 Mar 2015 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(comm_server).

-behaviour(gen_server).

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("RcsRpsMsg.hrl").
-include("sdp.hrl").
%%增加模块独立性，屏蔽sdp向深层使用，如遇性能问题，可以下放至worker。
-include("media_desc.hrl").
-include("audio_core.hrl").
-include("video_core.hrl").

-define(SYNC, <<255,255,255,0>>).

-define(TYPE_REG, 1).
-define(TYPE_IRCP, 11).

-define(CODE_REG, 1).
-define(CODE_ACK, 3).
-define(CODE_IRCP, 0).

%%-define(TYPE_IVR_STOP_ALL, 0).
-define(TYPE_UNKNOWN_STOP, 0).
-define(TYPE_IVR_STOP_PA, 1).
-define(TYPE_IVR_STOP_PC, 2).
-define(TYPE_IVR_STOP_PR, 3).
-define(TYPE_CONFPA_STOP, 7).
-define(TYPE_CONFPC_STOP, 8).
-define(TYPE_CONFPR_STOP, 9).

-define(SERVER, ?MODULE).

-include("status.hrl").

%%common IRCP Return Code
-define(IRCP_OPERATION_SUCCESS, 100).
-define(IRCP_RPS_INNER_ERROR, 400).
-define(IRCP_OPERATION_FAIL, 401).
-define(IRCP_SDP_NEGOTIATE_FAIL, 402).
-define(IRCP_UNSUPPORT_OPERATION, 441).
-define(IRCP_FILE_NOT_EXIST, 328).
-define(IRCP_OPERATION_STOP, 339).
-define(IRCP_UNSUPPORT_FILE_FORMAT, 998).

%%addRep
-define(IRCP_ADD_TBCP_PORT_GET_FAIL, 201).
-define(IRCP_ADD_TBCP_PORT_ALLOC_FAIL, 203).
-define(IRCP_ADD_ALREADY_IN_ROOM, 205).
-define(IRCP_ADD_NO_RES, 403).
-define(IRCP_ADD_CALLID_EXIST, 404).
-define(IRCP_ADD_GET_FILE_FAIL, 420).

%%subRep
-define(IRCP_SUB_NO_ENDPOINT, 500).
-define(IRCP_SUB_REL_TIMEOUT_FAIL, 555).

%%moveRep
-define(IRCP_CONFERENCE_JOIN_SUCCESS, 100).
-define(IRCP_CONFERENCE_JOIN_FAIL, 104).
-define(IRCP_CONFERENCE_UNJOIN_SUCCESS, 200).
-define(IRCP_CONFERENCE_UNJOIN_FAIL, 204).
-define(IRCP_CONFERENCE_SWITCH_SUCCESS, 300).
-define(IRCP_CONFERENCE_SWITCH_FAIL, 304).
-define(IRCP_CONFERENCE_ROOMID_ERR, 402).
-define(IRCP_CONFERENCE_TBCP_SWITCH_FAIL, 500).

%%bridgeRep
-define(IRCP_BRIDGE_SUCCESS, 100).
-define(IRCP_BRIDGE_FAIL, 101).
-define(IRCP_UNBRIDGE_SUCCESS, 200).
-define(IRCP_UNBRIDGE_FAIL, 201).

%%notify
-define(IRCP_NOTIFY_ADDCONFIRM_FAIL, 403).
-define(IRCP_NOTIFY_TIMEOUT_RELEASE, 500).
-define(IRCP_NOTIFY_TIMEOUT, 550).
-define(IRCP_NOTIFY_BOARD_RELEASE, 555).

%%stopRep
-define(IRCP_STOP_IVR_SUCCESS, 100).

%%for pa Reply
-define(IRCP_PA_END, 100).
-define(IRCP_PA_INTERUPT, 108).
-define(IRCP_PA_MAXTIME, 109).
-define(IRCP_PA_TTS_CREATEFILE_FAIL, 300).

%%for pc Reply
-define(IRCP_PC_NOINPUT_INTERUPT, 336).
-define(IRCP_PC_NOINPUT_NO_INTERUPT, 337).
-define(IRCP_PC_NOMATCH, 338).

%%for pr Reply
-define(IRCP_PR_SUC_POSTSPEECH, 100).
-define(IRCP_PR_FAL_PRESPEECH, 107).
-define(IRCP_PR_SUC_TERMKEY, 108).
-define(IRCP_PR_SUC_MAXTIME, 109).

%%createroomRep
-define(IRCP_CONFERENCE_CREATE_TBCP_FAIL, 150).
-define(IRCP_CONFERENCE_CREATE_NO_RES, 403).

%%channelRep
-define(IRCP_CHANNEL_H245_OPEN_SUCCESS, 100).
-define(IRCP_CHANNEL_H245_OPEN_FAIL, 101).
-define(IRCP_CHANNEL_H245_CLOSE_SUCCESS, 200).
-define(IRCP_CHANNEL_H245_CLOSE_FAIL, 201).
-define(IRCP_CHANNEL_CAMERA_OPEN_SUCCESS, 300).
-define(IRCP_CHANNEL_CAMERA_OPEN_FAIL, 301).

-record(state, { socket
               , rest = <<>>
               , seq
               , dfi
               , initsdp}).

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
start_link(Socket, RcsMsgr, Rcs, RpsMsgr, Rps, InitSdp) ->
    gen_server:start_link(?MODULE, [Socket, RcsMsgr, Rcs, RpsMsgr, Rps, InitSdp], []).

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
init([Socket, RcsMsgr, Rcs, RpsMsgr, Rps, InitSdp]) ->
    process_flag(trap_exit, true),
    erlang:port_connect(Socket, self()),
    case inet:setopts(Socket, [{active, true}]) of
        {error, R} ->
            exit(R);
        _ ->
            ok
    end,
    BinDest = list_to_binary(tuple_to_list(RcsMsgr)),
    BinSrc = list_to_binary(tuple_to_list(RpsMsgr)),
    comm_send(Socket, ?TYPE_REG, ?CODE_REG, BinDest, BinSrc, 0, <<>>),
    {ok, #state{socket = Socket, seq = 1, dfi = {RcsMsgr, RpsMsgr, Rcs, Rps}, initsdp = InitSdp}}.

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
handle_cast({testAlive, _CallID, AliveStatus, _}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    {DFI_D, _, DFI_I} = Rps,
    {Total, Using, Bad, TotalConf, UsingConf} = AliveStatus,
    ResInfo = {'resinfo', #'ResInfo'{ totalRes = Total
                                    , usingRes = Using
                                    , badUseNo = Bad
                                    , totalConf = TotalConf
                                    , usingConf = UsingConf
                                    , identifyID = DFI_D}},
    TermInfo = #'TerminationStruct'{ rpsNo = DFI_D*100 + DFI_I
                                   , boardNo = 0
                                   , channelNo = 0},
    Ircp = #'RcsRpsMessage'{ transactionID = 0
                           , contextID = 0
                           , terminationID = TermInfo
                           , endpointID = TermInfo
                           , callID = 0
                           , command = ResInfo},
    %%info_manager:info("testAlive response ~p~n", [Ircp]),
    ircp_send(Socket, Ircp, 0, Rcs, Rps),
    {noreply, State#state{seq = Seq +1}};

handle_cast( {alloc_media, CallID, {error, ebadport},  {AllocPara, MetaData}}, State) ->
    Alloc = {realloc_media, CallID, AllocPara, MetaData},
    worker_pool:cast(Alloc),
    {noreply, State};

handle_cast( {alloc_media, CallID, Result, {rcs, RcsReq, SDPStruct, TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    {DFI_D, _, DFI_I} = Rps,
    case Result of
        {ok, IPVer, IP, Port} ->
            info_manager:req(req_alloc_succ),
            SDP = case Port of
                      [AudioPort] ->
                           sdp:generate(SDPStruct#sdp{ addrtype = IPVer
                                                     , addr = IP
                                                     , audio = (SDPStruct#sdp.audio)#media_descript{port = AudioPort}});
                      [AudioPort, VideoPort] ->
                           sdp:generate(SDPStruct#sdp{ addrtype = IPVer
                                                     , addr = IP
                                                     , audio = (SDPStruct#sdp.audio)#media_descript{port = AudioPort}
                                                     , video = (SDPStruct#sdp.video)#media_descript{port = VideoPort}})
                  end,
            {ReturnCode, NSDP} = case SDP of
                {ok, SdpStr} ->
                    {?IRCP_OPERATION_SUCCESS, SdpStr};
                {error, _R} ->
                    {?IRCP_SDP_NEGOTIATE_FAIL, ""}
            end,
            %%info_manager:info("200 sdp: ~p~n", [NSDP]), %%rcs会改变200sdp中a行的顺序
            EndID = #'TerminationStruct'{ rpsNo = DFI_D*100 + DFI_I
                                        , boardNo = 0
                                        , channelNo = CallID
                                        },
            Command = {'addRep', #'IntraAddReply'{ addReturn = ReturnCode
                                                 , localSDP = NSDP}},
            Ircp = (ircp_gen(Command, RcsReq))#'RcsRpsMessage'{endpointID = EndID},
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            info_manager:time(time_alloc_expend, TimerStart),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_alloc_fail),
            ReturnCode = case Reason of
                eworkermax ->
                    ?IRCP_ADD_NO_RES;
                eworkerexist ->
                    ?IRCP_ADD_CALLID_EXIST;
                negofail ->
                    ?IRCP_SDP_NEGOTIATE_FAIL;
                ebadport ->
                    ?IRCP_ADD_TBCP_PORT_ALLOC_FAIL;
                _ ->
                    ?IRCP_ADD_NO_RES
            end,
            %% need test: 这里的endpointID不用重新生成么？
            Command = {'addRep', #'IntraAddReply'{ addReturn = ReturnCode
                                                 , localSDP = ""}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;
handle_cast( {alloc_media_ack, CallID, {error, Reason}, {rcs, RcsReq, none}}, #state{seq = Seq} = State) ->
    info_manager:req(req_ack_fail),
    info_manager:log_error("Ack error", io_lib:format("~p", [Reason])),
    info_manager:info("ack error reason ~p~n", [Reason]),
    worker_pool:cast({release_media, CallID, none, {rcs, RcsReq}}),
    {noreply, State#state{seq = Seq +1}};

handle_cast( {release_media, _CallID, _, {rcs, RcsReq, TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    info_manager:req(req_release_succ),
    {_,_, Rcs, Rps} = DFI,
    Command = {'subtractRep', #'IntraSubReply'{subReturn = ?IRCP_OPERATION_SUCCESS}},
    Ircp = ircp_gen(Command, RcsReq),
    ircp_send(Socket, Ircp, Seq, Rcs, Rps),
    info_manager:time(time_release_expend, TimerStart),
    {noreply, State#state{seq = Seq +1}};
handle_cast({play_media, _CallID, Result, {rcs, RcsReq, _TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok, over, PlayingTime} ->
            info_manager:req(req_play_succ),
            AuOc = #'AuOc'{ auOcReturn = { 'auPaReturnCode'
                                         , #'PaReturnParam'{ returnCode = 100
                                                           , pt = integer_to_list(PlayingTime div 1000000)}}},

            Command = {'eventRep', #'IntraEvent'{ eventType = 0
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_play_fail),
            ReturnCode = case Reason of
                enoent ->
                    ?IRCP_FILE_NOT_EXIST;
                eformat ->
                    ?IRCP_UNSUPPORT_FILE_FORMAT;
                epattern ->
                    ?IRCP_RPS_INNER_ERROR;
                _ ->
                    ?IRCP_RPS_INNER_ERROR
            end,
            AuOc = #'AuOc'{ auOcReturn = { 'auPaReturnCode'
                                         , #'PaReturnParam'{ returnCode = ReturnCode
                                                           , pt = "0"}}},
            Command = {'eventRep', #'IntraEvent'{ eventType = 0
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;
handle_cast({unknown_stop, CallID, Result, MetaData}, State) ->
    info_manager:req(req_stop_unknown_succ),
    handle_cast({stop, CallID, Result, MetaData}, State);
handle_cast({IvrStop, CallID, Result, MetaData}, State) when IvrStop =:= stop_pa
                                                           ; IvrStop =:= stop_pc
                                                           ; IvrStop =:= stop_pr ->
    info_manager:req(req_stop_ivr_succ),
    handle_cast({stop, CallID, Result, MetaData}, State);
handle_cast({ConfStop, CallID, Result, MetaData}, State) when ConfStop =:= stop_confpa_media
                                                            ; ConfStop =:= stop_conf_dc
                                                            ; ConfStop =:= stop_confpr_media ->
    report_req_conf_stop(ConfStop),
    handle_cast({stop, CallID, Result, MetaData}, State);
handle_cast({stop, _CallID, _Result, {rcs, RcsReq}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    Command = {'stopRep', #'IntraStopReply'{ operationID = -1
                                           , stopReturn =  ?IRCP_STOP_IVR_SUCCESS}},
    Ircp = ircp_gen(Command, RcsReq),
    ircp_send(Socket, Ircp, Seq, Rcs, Rps),
    {noreply, State#state{seq = Seq +1}};
%%handle_cast({Type, _CallID, [], {rcs, RcsReq}}
%%           , #state{ socket = Socket
%%                   , seq = Seq
%%                   , dfi = DFI} = State) when Type =:= stop_pa
%%                                            ; Type =:= stop_pc
%%                                            ; Type =:= stop_pr ->
%%    info_manager:info("stop ivr succ, type:~p~n", [Type]),
%%    info_manager:req(req_stop_all_succ),
%%    {_,_, Rcs, Rps} = DFI,
%%    Command = {'stopRep', #'IntraStopReply'{ operationID = -1
%%                                           , stopReturn =  ?IRCP_STOP_IVR_SUCCESS}},
%%    Ircp = ircp_gen(Command, RcsReq),
%%    ircp_send(Socket, Ircp, Seq, Rcs, Rps),
%%    {noreply, State#state{seq = Seq +1}};
handle_cast({digit_collect, _CallID, Result, {rcs, RcsReq, _TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok, Return, Digits} ->
            info_manager:req(req_pc_succ),
            ReturnCode = case Return of
                wrong_length ->
                    ?IRCP_PC_NOMATCH;
                nomatch ->
                    ?IRCP_PC_NOMATCH;
                noinput ->
                    %%如需要，再添加NO_INTERUPT
                    %%话说，收号时放音打不打断有什么区别
                    ?IRCP_PC_NOINPUT_INTERUPT;
                %%termkey ->
                %%match ->
                _ ->
                    ?IRCP_OPERATION_SUCCESS
            end,
            PcReturnParam = #'PcReturnParam' { returnCode = ReturnCode
                                             , dc = Digits},
            AuOc = #'AuOc'{ auOcReturn = { 'auPcReturnParam'
                                         , PcReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ eventType = 0
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_pc_fail),
            ReturnCode = case Reason of
                enoent ->
                    ?IRCP_FILE_NOT_EXIST;
                eformat ->
                    ?IRCP_UNSUPPORT_FILE_FORMAT;
                epattern ->
                    %%正则错误，暂时只能用这个顶一下
                    ?IRCP_PC_NOMATCH;
                _ ->
                    ?IRCP_RPS_INNER_ERROR
            end,
            PcReturnParam = #'PcReturnParam' { returnCode = ReturnCode
                                             , dc = ""},
            AuOc = #'AuOc'{ auOcReturn = { 'auPcReturnParam'
                                         , PcReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ eventType = 0
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;

handle_cast({record_media, _CallID, Result, {rcs, RcsReq, _TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok, Return, TimeLen} ->
            info_manager:req(req_pr_succ),
            ReturnCode = case Return of
                termkey ->
                    ?IRCP_PR_SUC_TERMKEY;
                maxtime ->
                    ?IRCP_PR_SUC_MAXTIME;
                _ ->
                    ?IRCP_PR_SUC_POSTSPEECH
            end,
            PrReturnParam = #'PrReturnParam' { returnCode = ReturnCode
                                             , rt = integer_to_list(TimeLen)},
            AuOc = #'AuOc'{ auOcReturn = { 'auPrReturnParam'
                                         , PrReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ eventType = 0
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_pr_fail),
            ReturnCode = case Reason of
                enoent ->
                    ?IRCP_FILE_NOT_EXIST;
                eformat ->
                    ?IRCP_UNSUPPORT_FILE_FORMAT;
                _ ->
                    ?IRCP_RPS_INNER_ERROR
            end,
            PrReturnParam = #'PrReturnParam' { returnCode = ReturnCode
                                             , rt = "0"},
            AuOc = #'AuOc'{ auOcReturn = { 'auPrReturnParam'
                                         , PrReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ eventType = 0
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;

handle_cast( {alloc_conf, _RoomID, Result, {rcs, RcsReq}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    {DFI_D, _, DFI_I} = Rps,
    CallID = RcsReq#'RcsRpsMessage'.callID,
    %%按罗诚说，某个版本rcs改版，之后所有的关于会议的操作都会以createroom
    %%开始新启动一个会话，所以EndID得重新来一遍
    case Result of
        {ok} ->
            info_manager:req(req_create_succ),
            EndID = #'TerminationStruct'{ rpsNo = DFI_D*100 + DFI_I
                                        , boardNo = 0
                                        , channelNo = CallID
                                        },
            Command = {creatroomRep,  ?IRCP_OPERATION_SUCCESS},
            Ircp = (ircp_gen(Command, RcsReq))#'RcsRpsMessage'{endpointID = EndID},
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_create_fail),
            ReturnCode = case Reason of
                eworkermax ->
                    ?IRCP_CONFERENCE_CREATE_NO_RES;
                _ ->
                    ?IRCP_CONFERENCE_CREATE_NO_RES
            end,
            %% need test: 这里的endpointID不用重新生成么？
            Command = {creatroomRep, ReturnCode},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;
handle_cast( {release_conf, _RoomID, Result, {rcs, RcsReq}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok} ->
            info_manager:req(req_destroy_succ),
            Command = {delroomRep,  ?IRCP_OPERATION_SUCCESS},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_destroy_succ),
            ReturnCode = case Reason of
                exist ->
                    ?IRCP_CONFERENCE_ROOMID_ERR;
                _ ->
                    ?IRCP_CONFERENCE_ROOMID_ERR
            end,
            Command = {delroomRep, ReturnCode},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;
handle_cast( {join_conf, _RoomID, Result, {rcs, RcsReq}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok} ->
            info_manager:req(req_join_succ),
            Command = {moveRep,  ?IRCP_CONFERENCE_JOIN_SUCCESS},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_join_fail),
            ReturnCode = case Reason of
                exist ->
                    ?IRCP_CONFERENCE_ROOMID_ERR;
                ejoinmax ->
                    ?IRCP_CONFERENCE_JOIN_FAIL;
                _ ->
                    ?IRCP_CONFERENCE_JOIN_FAIL
            end,
            Command = {moveRep, ReturnCode},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;
handle_cast( {unjoin_conf, _RoomID, Result, {rcs, RcsReq}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok} ->
            info_manager:req(req_unjoin_succ),
            Command = {moveRep,  ?IRCP_CONFERENCE_UNJOIN_SUCCESS},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_unjoin_fail),
            ReturnCode = case Reason of
                exist ->
                    ?IRCP_CONFERENCE_ROOMID_ERR;
                ejoinmax ->
                    ?IRCP_CONFERENCE_JOIN_FAIL;
                _ ->
                    ?IRCP_CONFERENCE_JOIN_FAIL
            end,
            Command = {moveRep, ReturnCode},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;
handle_cast({conf_play, RoomID, Result, {rcs, RcsReq, _TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok, over, PlayingTime} ->
            info_manager:req(req_conf_pa_succ),
            AuOc = #'AuOc'{ auOcReturn = { 'confPaReturnCode'
                                         , #'PaReturnParam'{ returnCode = 100
                                                           , pt = integer_to_list(PlayingTime div 1000000)}}},

            Command = {'eventRep', #'IntraEvent'{ roomID = RoomID
                                                , eventType = 7
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq + 1}};
        {error, Reason} ->
            info_manager:req(req_conf_pa_fail),
            ReturnCode = case Reason of
                enoent ->
                    ?IRCP_FILE_NOT_EXIST;
                eformat ->
                    ?IRCP_UNSUPPORT_FILE_FORMAT;
                epattern ->
                    ?IRCP_RPS_INNER_ERROR;
                _ ->
                    ?IRCP_RPS_INNER_ERROR
            end,
            AuOc = #'AuOc'{ auOcReturn = { 'confPaReturnCode'
                                         , #'PaReturnParam'{ returnCode = ReturnCode
                                                           , pt = "0"}}},
            Command = {'eventRep', #'IntraEvent'{ roomID = RoomID
                                                , eventType = 7
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq + 1}}
    end;

%%handle_cast({ConfStop, _CallID, _Result, {rcs, RcsReq}}
%%           , #state{ socket = Socket
%%                   , seq = Seq
%%                   , dfi = DFI} = State) when ConfStop =:= stop_confpa_media
%%                                            ; ConfStop =:= stop_conf_dc
%%                                            ; ConfStop =:= stop_confpr_media ->
%%    report_req_conf_stop(ConfStop),
%%    {_,_, Rcs, Rps} = DFI,
%%    Command = {'stopRep', #'IntraStopReply'{ operationID = -1
%%                                           , stopReturn =  ?IRCP_STOP_IVR_SUCCESS}},
%%    Ircp = ircp_gen(Command, RcsReq),
%%    ircp_send(Socket, Ircp, Seq, Rcs, Rps),
%%    {noreply, State#state{seq = Seq +1}};
handle_cast({conf_digit_collect, RoomID, Result, {rcs, RcsReq, _TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok, Return, Digits} ->
            info_manager:req(req_conf_pc_succ),
            ReturnCode = case Return of
                wrong_length ->
                    ?IRCP_PC_NOMATCH;
                nomatch ->
                    ?IRCP_PC_NOMATCH;
                noinput ->
                    %%如需要，再添加NO_INTERUPT
                    %%话说，收号时放音打不打断有什么区别
                    ?IRCP_PC_NOINPUT_INTERUPT;
                %%termkey ->
                %%match ->
                _ ->
                    ?IRCP_OPERATION_SUCCESS
            end,
            PcReturnParam = #'PcReturnParam' { returnCode = ReturnCode
                                             , dc = Digits},
            %%后期合并代码注意，这里是confPcReturnParam，与之前auPc有区别
            AuOc = #'AuOc'{ auOcReturn = { 'confPcReturnParam'
                                         , PcReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ roomID = RoomID
                                                , eventType = 7
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_conf_pc_fail),
            ReturnCode = case Reason of
                enoent ->
                    ?IRCP_FILE_NOT_EXIST;
                eformat ->
                    ?IRCP_UNSUPPORT_FILE_FORMAT;
                epattern ->
                    %%正则错误，暂时只能用这个顶一下
                    ?IRCP_PC_NOMATCH;
                _ ->
                    ?IRCP_RPS_INNER_ERROR
            end,
            PcReturnParam = #'PcReturnParam' { returnCode = ReturnCode
                                             , dc = ""},
            AuOc = #'AuOc'{ auOcReturn = { 'confPcReturnParam'
                                         , PcReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ roomID = RoomID
                                                , eventType = 7
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;

handle_cast({conf_record_media, RoomID, Result, {rcs, RcsReq, _TimerStart}}
           , #state{ socket = Socket
                   , seq = Seq
                   , dfi = DFI} = State) ->
    {_,_, Rcs, Rps} = DFI,
    case Result of
        {ok, Return, TimeLen} ->
            info_manager:req(req_conf_rec_succ),
            ReturnCode = case Return of
                termkey ->
                    ?IRCP_PR_SUC_TERMKEY;
                maxtime ->
                    ?IRCP_PR_SUC_MAXTIME;
                _ ->
                    ?IRCP_PR_SUC_POSTSPEECH
            end,
            PrReturnParam = #'PrReturnParam' { returnCode = ReturnCode
                                                 , rt = integer_to_list(TimeLen)},
            %%后期合并代码注意，这里是confPrReturnParam，与之前auPr有区别
            AuOc = #'AuOc'{ auOcReturn = { 'confPrReturnParam'
                                         , PrReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ roomID = RoomID
                                                , eventType = 7
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}};
        {error, Reason} ->
            info_manager:req(req_conf_rec_fail),
            ReturnCode = case Reason of
                enoent ->
                    ?IRCP_FILE_NOT_EXIST;
                eformat ->
                    ?IRCP_UNSUPPORT_FILE_FORMAT;
                _ ->
                    ?IRCP_RPS_INNER_ERROR
            end,
            PrReturnParam = #'PrReturnParam' { returnCode = ReturnCode
                                             , rt = "0"},
            AuOc = #'AuOc'{ auOcReturn = { 'confPrReturnParam'
                                         , PrReturnParam}},
            Command = {'eventRep', #'IntraEvent'{ roomID = RoomID
                                                , eventType = 7
                                                , auOc = AuOc}},
            Ircp = ircp_gen(Command, RcsReq),
            ircp_send(Socket, Ircp, Seq, Rcs, Rps),
            {noreply, State#state{seq = Seq +1}}
    end;

handle_cast(Msg, State) ->
    info_manager:info("~p unexpect msg ~p",[?MODULE, Msg]),
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
handle_info({tcp, Socket, Bin}, #state{ socket = Socket
                                      , rest = Rest
                                      , dfi = DFI
                                      , initsdp = InitSdp} = State) ->
    %%info_manager:info("==================================================~n"),
    NRest = run_comm(<<Rest/binary, Bin/binary>>, DFI, InitSdp, Socket),
    %%完成时这里注意增加try模块，丢弃内部错误包，记日志，维持稳定性
    %%info_manager:info("--->: ~p~n", [CommRep]),
    %%gen_tcp:send(Socket, CommRep),
    {noreply, State#state{rest = NRest}};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    info_manager:info("comm server socket closed"),
    info_manager:log_error("Socket link broken",""),
    {stop, {shutdown, tcp_closed}, State};
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
comm_encode(Type, Code, D3,S3, Seq, Content) ->
    Len = byte_size(Content) + 23,
    Vsn = 1,
    Reserve = 0,
    EOS = <<0:1,1:7>>,
    TTL = 4,
    Session = <<" IMP">>,
    << ?SYNC/binary, Vsn:8, Reserve:8, Len:16
     , Type:8, Code:8, EOS/binary
     , S3/binary, Session/binary
     , D3/binary, Session/binary
     , Seq:8
     , TTL:8
     , Content/binary>>.

comm_decode(<<>>) ->
    {none, <<>>};
comm_decode(Bin) when byte_size(Bin) < 23 ->
    {none, Bin};
comm_decode(<<Sync:4/binary, _:2/binary, Len:16, _/binary>> = Bin) when Sync =:= ?SYNC, Len + 4 > byte_size(Bin) ->
    {none, Bin};

comm_decode(<<Sync:4/binary, _:2/binary, Len:16, _/binary>> = Bin) when Sync =:= ?SYNC ->
    <<_:4/binary, MsgBin:Len/binary, Remain/binary>> = Bin,
    << _Vsn:8, _Reserve:8, _Len:16
     , Type:8, Code:8, _EOS:1/binary
     , _S3:3/binary, _SessionS:4/binary
     , D3:3/binary, _SessionD:4/binary
     , _Seq:8
     , _TTL:8
     , Content/binary>> = MsgBin,
    <<D,F,I>> = D3,
    {{{Type, Code, {D,F,I}}, Content}, Remain};
comm_decode(Bin) ->
    <<Drop:1/binary, R/binary>> = Bin,
    error_logger:info_msg("drop comm byte ~p",[Drop]),
    comm_decode(R).

run_comm(Bin, DFI, InitSdp, Socket)->
    case comm_decode(Bin) of
        {none, Remain} ->
            Remain;
        {CommReq, Remain} ->
            process_comm(CommReq, DFI, InitSdp, Socket),
            run_comm(Remain, DFI, InitSdp, Socket)
    end.

process_comm({Head, Content}, {RcsMsgr, RpsMsgr, _, Rps}, InitSdp, Socket) ->
   case
    case Head of
        {?TYPE_REG, Code, _} ->
            process_msgr(Code, RcsMsgr, RpsMsgr);
        {?TYPE_IRCP, ?CODE_IRCP, MsgDFI} when MsgDFI =:= Rps ->
            process_rps(Content, InitSdp);
        _ ->
            error_logger:info_msg("bad comm ~p",[{Head, Content}]),
            {unknown, drop}
    end
   of
     {worker, WorkerCommand} ->
         worker_pool:cast(WorkerCommand);
     {conf, ConfCommand} ->
         conf_pool:cast(ConfCommand);
     {rcs, RcsResponse} ->
         gen_server:cast(self(), RcsResponse);
     {worker_stop, StopCommand} ->
         gen_server:cast(self(), StopCommand),
         worker_pool:cast(StopCommand);
     {conf_stop, StopCommand} ->
         gen_server:cast(self(), StopCommand),
         conf_pool:cast(StopCommand);
     {unknown_stop, StopCommand} ->
         gen_server:cast(self(), StopCommand);
     {socket, Bin} ->
         gen_tcp:send(Socket, Bin);
     {none, _} ->
         ok;
     {unknown, _} ->
         ok
   end.

process_msgr(Code, RcsMsgr, RpsMsgr) when Code =:= ?CODE_REG ->
    {socket, comm_encode(?TYPE_REG, ?CODE_ACK, list_to_binary(tuple_to_list(RcsMsgr)), list_to_binary(tuple_to_list(RpsMsgr)), 2, <<>>)};
process_msgr(Code, _, _) when Code =:= ?CODE_ACK ->
    {none, ack};
process_msgr(Code, _, _) ->
    error_logger:info_msg("bad msgr code ~p",[Code]),
    {unknown, drop}.

process_rps(Content, InitSdp) ->
    {ok, RcsReq} = 'RcsRpsMsg':decode('RcsRpsMessage', Content),
    #'RcsRpsMessage' { callID = CallID
                     , command = Command} = RcsReq,
    process_command(Command, InitSdp, RcsReq, CallID).

process_command({testAlive, _}, _InitSdp, RcsReq, CallID) ->
    MetaData = {rcs, RcsReq},
    #status{ worker_max = Total
           , worker_running = Using
           , worker_blocked = Bad
           , conf_max = TotalConf
           , conf_running = UsingConf} = status:get_status(),
    AliveStatus = {Total, Using, Bad, TotalConf, UsingConf},
    {rcs, {testAlive, CallID, AliveStatus, MetaData}};
process_command({addReq, _} = Command, InitSdp, RcsReq, CallID) ->
    %%info_manager:info("addReq ~p~n", [RcsReq]),
    info_manager:req(req_alloc_total),
    TimerStart = os:timestamp(),
    {'addReq', #'IntraAdd'{remoteSDP = SDP}} = Command,
    case sdp:parse_remote_sdp(SDP) of
        {ok, RemoteSdp} ->
            case sdp:negotiate(RemoteSdp, InitSdp) of
                {ok, SDPNego} ->
                    %%info_manager:info("RemoteSdp ~p~n", [RemoteSdp]),
                    %%info_manager:info("InitSdp ~p~n", [InitSdp]),
                    %%info_manager:info("SDPNego ~p~n", [SDPNego]),
                    #sdp{ audio = AudioDescript
                        , video = VideoDescript} = SDPNego,
                    #media_descript{addrtype = IPVersion, addr = IP} = AudioDescript,
                    PortNeed = media_need_port(AudioDescript)
                             + media_need_port(VideoDescript),
                    MetaData = {rcs, RcsReq, SDPNego, TimerStart},
                    IPVer = case IPVersion of
                                inet -> inet;
                                inet6 -> inet6;
                                {_, {default, Ver}} -> Ver
                            end,
                    AllocPara = { {port_need, PortNeed}
                                , { {addr, IPVer, IP}
                                  , trans_audio(AudioDescript)
                                  , trans_video(VideoDescript)}},
                    {worker, {alloc_media, CallID, AllocPara, MetaData}};
                {error, Reason} ->
                    info_manager:log_error("Negotiate sdp error", io_lib:format("~p", [Reason])),
                    info_manager:info("negotiate sdp error reason ~p~n", [Reason]),
                    {rcs, {alloc_media, CallID, {error, negofail}, {rcs, RcsReq, none, TimerStart}}}
            end;
        {error, R} ->
            info_manager:log_error("Parse sdp error", io_lib:format("~p", [R])),
            info_manager:info("parse sdp error reason ~p~n", [R]),
            {rcs, {alloc_media, CallID, {error, negofail}, {rcs, RcsReq, none, TimerStart}}}
    end;
process_command({addConfirm, IntraAddConfirm}, _InitSdp, RcsReq, CallID) ->
    info_manager:req(req_ack_total),
    TimerStart = os:timestamp(),
    SDP = IntraAddConfirm#'IntraAddConfirm'.remoteSDP,
    case sdp:parse_remote_sdp(SDP) of
        {ok, none} ->
            info_manager:req(req_ack_succ),
            SDPNego = none,
            AckPara = none,
            MetaData = {rcs, RcsReq, SDPNego, TimerStart},
            {worker, {alloc_media_ack, CallID, AckPara, MetaData}};
        {ok, RemoteSdp} ->
            #sdp{ audio = AudioDescript
                , video = VideoDescript} = RemoteSdp,
            #media_descript{addrtype = IPVersion, addr = IP} = AudioDescript,
            IPVer = case IPVersion of
                        inet -> inet;
                        inet6 -> inet6;
                        {_, {default, Ver}} -> Ver
                    end,
            case {trans_audio(AudioDescript), trans_video(VideoDescript)} of
                {AudioDesc, VideoDesc} when AudioDesc#audio_desc.codec =:= unsupport
                                          ; VideoDesc#video_desc.codec =:= unsupport ->
                    {rcs, {alloc_media_ack, CallID, {error, codec_unsupport}, {rcs, RcsReq, none}}};
                {AudioDesc, VideoDesc} ->
                    info_manager:req(req_ack_succ),
                    AckPara = { {port_need, 0}
                                , { {addr, IPVer, IP}
                                   , AudioDesc
                                   , VideoDesc}},
                    MetaData = {rcs, RcsReq, RemoteSdp, TimerStart},
                    {worker, {alloc_media_ack, CallID, AckPara, MetaData}}
            end;
            %%case sdp:negotiate(RemoteSdp, InitSdp) of
            %%    {ok, SDPNego} ->
            %%        %%info_manager:info("Ack RemoteSdp ~p~n", [RemoteSdp]),
            %%        %%info_manager:info("Ack InitSdp ~p~n", [InitSdp]),
            %%        %%info_manager:info("Ack SDPNego ~p~n", [SDPNego]),
            %%        #sdp{ addrtype = IPVersion
            %%            , addr = IP
            %%            , audio = AudioDescript
            %%            , video = VideoDescript} = SDPNego,
            %%        MetaData = {rcs, RcsReq, SDPNego},
            %%        IPVer = case IPVersion of
            %%                    inet -> inet;
            %%                    inet6 -> inet6;
            %%                    {_, {default, Ver}} -> Ver
            %%                end,
            %%        AckPara = { {port_need, 0}
            %%                    , { {addr, IPVer, IP}
            %%                      , trans_audio(AudioDescript)
            %%                      , trans_video(VideoDescript)}},
            %%        {worker, {alloc_media_ack, CallID, AckPara, MetaData}};
            %%    {error, _Reason} ->
            %%        {rcs, {alloc_media_ack, CallID, {error, negofail}, {rcs, RcsReq, none}}}
            %%end;
        {error, R} ->
            %%worker 应该停止，或设定某些标志，后续除释放操作皆失败，暂未实现
            info_manager:log_error("Parse ack sdp error", io_lib:format("~p", [R])),
            info_manager:info("parse ack sdp error reason ~p~n", [R]),
            {rcs, {alloc_media_ack, CallID, {error, negofail}, {rcs, RcsReq, none}}}
    end;
process_command({subtractReq, _}, _InitSdp, RcsReq, CallID) ->
    info_manager:req(req_release_total),
    TimerStart = os:timestamp(),
    ReleasePara = none,
    MetaData = {rcs, RcsReq, TimerStart},
    {worker, {release_media, CallID, ReleasePara, MetaData}};
process_command({signalReq, _} = Command, _InitSdp, RcsReq, CallID) ->
    info_manager:info("signalReq ~p~n", [RcsReq]),
    {'signalReq', #'IntraSignal'{signal = Signal}} = Command,
    TimerStart = os:timestamp(),
    process_signal(Signal, RcsReq, CallID, TimerStart);
process_command({stopReq, Type}, _InitSdp, RcsReq, CallID) ->
    info_manager:info("stopReq ~p~n", [RcsReq]),
    case Type of
        ?TYPE_IVR_STOP_PA ->
            info_manager:req(req_stop_ivr_total),
            {worker_stop, {stop_pa, CallID, [], {rcs, RcsReq}}};
        ?TYPE_IVR_STOP_PC ->
            info_manager:req(req_stop_ivr_total),
            {worker_stop, {stop_pc, CallID, [], {rcs, RcsReq}}};
        ?TYPE_IVR_STOP_PR ->
            info_manager:req(req_stop_ivr_total),
            {worker_stop, {stop_pr, CallID, [], {rcs, RcsReq}}};
        ?TYPE_CONFPA_STOP ->
            info_manager:req(req_conf_stop_pa_total),
            {conf_stop, {stop_confpa_media, {callid, CallID}, [], {rcs, RcsReq}}};
        ?TYPE_CONFPC_STOP ->
            info_manager:req(req_conf_stop_pc_total),
            {conf_stop, {stop_conf_dc, {callid, CallID}, [], {rcs, RcsReq}}};
        ?TYPE_CONFPR_STOP ->
            info_manager:req(req_conf_stop_rec_total),
            {conf_stop, {stop_confpr_media, {callid, CallID}, [], {rcs, RcsReq}}};
        ?TYPE_UNKNOWN_STOP ->
            info_manager:req(req_stop_unknown_total),
            {unknown_stop, {unknown_stop, CallID, [], {rcs, RcsReq}}}
    end;
process_command({creatroomReq, {audioConf, ConfPara}}, _InitSdp, RcsReq, CallID) ->
    info_manager:info("audio conf ~p", [ConfPara]),
    info_manager:req(req_create_total),
    #'IntraConfPara'{roomID = RoomID} = ConfPara,
    AllocPara = none,
    MetaData = {rcs, RcsReq},
    {conf, {alloc_conf, {CallID, RoomID}, AllocPara, MetaData}};
process_command({delroomReq, #'IntraDeleteRoom'{roomID = RoomID}}, _InitSdp, RcsReq, CallID) ->
    info_manager:info("del conf ~p", [RoomID]),
    info_manager:req(req_destroy_total),
    ReleasePara= none,
    MetaData = {rcs, RcsReq},
    {conf, {release_conf, {CallID, RoomID}, ReleasePara, MetaData}};
process_command({moveReq, #'IntraMove'{ destRoomID = DRoomID
                                      , sourceRoomID = SRoomID}}, _InitSdp, RcsReq, _CallID) ->
    info_manager:info("rcsreq ~p", [RcsReq]),
    #'RcsRpsMessage'{terminationID =
                     #'TerminationStruct'{channelNo = MemberID}} = RcsReq,
    info_manager:info("join ~p from ~p to conf ~p", [MemberID, SRoomID, DRoomID]),
    JoinPara = {member, MemberID},
    MetaData = {rcs, RcsReq},
    case {SRoomID, DRoomID} of
        {-1, _} ->
            info_manager:req(req_join_total),
            {conf, {join_conf, DRoomID, JoinPara, MetaData}};
        {_, -1} ->
            info_manager:req(req_unjoin_total),
            {conf, {unjoin_conf, SRoomID, JoinPara, MetaData}}
    end;
    %%其实就消息来讲，完全可以直接发给worker来处理加入操作，
    %%感觉上加入这个操作本身也应该属于worker的主动范畴，但是
    %%现在的设计就是会议拉一个人入伙，那就先实现成这样吧
process_command(Command, _InitSdp, _RcsReq, _CallID) ->
    error_logger:info_msg("bad rps command ~p",[Command]),
    {unknown, drop}.

process_signal({auPa, AuPa}, RcsReq, CallID, TimerStart) ->
    info_manager:req(req_play_total),
    #'AuPa'{ file = FileList
           , it = PlayTimesIt
           , iv = IntervalTime
           , du = TotalTime
           , ni = Ni} = AuPa,
    Files = lists:foldr(fun({filename, Name}, FilesIn) ->
                                [Name|FilesIn];
                           (_, FilesIn) ->
                                FilesIn
                        end, [], tuple_to_list(FileList)),
    Total = asn_to_value(time, TotalTime),
    Interval = asn_to_value(mintime, IntervalTime),
    PlayTimes = asn_to_value(times, PlayTimesIt),
    Barge = asn_to_value(barge, Ni),
    PlayPara = #media_ctrl{ type = audio
                          , filenames = Files
                          , times = PlayTimes
                          , interval = Interval
                          , max_time = Total
                          , barge = Barge},
    MetaData = {rcs, RcsReq, TimerStart},
    {worker, {play_media, CallID, PlayPara, MetaData}};
process_signal({auPc, AuPc}, RcsReq, CallID, TimerStart) ->
    info_manager:req(req_pc_total),
    #'AuPc'{ ni = Ni
           , ip = FileList
           , tp = AnPlayTimes
           , na = TimesRetry
           , fdt = FdtTime
           , idt = IdtTime
           , dp = DigitPattern
           , mn = MinDigit
           , mx = MaxDigit
           , rik = RikKey     %%重新开始收号
           , eik = EikKey     %%本次收号结束，不上报结束键
           %%, rsk = RskKey   %%当前整体放音收号重做，暂不支持
           %%, rtk = RtkKey
                    %%结束当前操作返回，
                    %%与Eik不同的是，单纯收号结束，
                    %%有可能是未打断继续放音的
                    %%暂时不支持，且当前收号结束时
                    %%也同时结束了放音
           , starttimer = StartFdtTimer} = AuPc,
    Barge = asn_to_value(barge, Ni),
    Fdt = asn_to_value(time, FdtTime),
    Idt = asn_to_value(time, IdtTime),
    Files = asn_to_value(filelist, FileList),
    Pattern = asn_to_value(pattern, DigitPattern),
    Min = asn_to_value(integer, MinDigit),
    Max = asn_to_value(integer, MaxDigit),
    Rik = asn_to_value(key, RikKey),
    Eik = asn_to_value(key, EikKey),
    Times = asn_to_value(times, TimesRetry),
    DCPara = #digit_ctrl{ timer = #digit_timer{ fdt = Fdt
                                              , idt = Idt
                                              , start_fdt_timer = StartFdtTimer}
                        , digit = case Pattern of
                                      none ->
                                          #digit_dgt{ min = Min
                                                    , max = Max
                                                    , rik = Rik
                                                    , eik = Eik};
                                      _ ->
                                          #digit_pattern{ pattern = Pattern}
                                  end
                        , media = case Files of
                                      [] ->
                                          none;
                                      _ ->
                                           #media_ctrl{ type = audio
                                                      , filenames = Files
                                                      , times = AnPlayTimes}
                                  end
                        , times = Times
                        , barge = Barge},
    MetaData = {rcs, RcsReq, TimerStart},
    {worker, {digit_collect, CallID, DCPara, MetaData}};
process_signal({auPr, AuPr}, RcsReq, CallID, TimerStart) ->
    info_manager:req(req_pr_total),
    #'AuPr'{ ip = FileList
           , rf = RecordFileName
           , ni = Ni
           %%, prt = PreSpeechTimer
           %%, pst = PostSpeechTimer
           , rlt = TotalTimeLen
           , eik = EikKey
           , rcdfmt = FormatString
           , append = _Append} = AuPr,

    Files = asn_to_value(filelist, FileList),
    RecordFile = asn_to_value(string, RecordFileName),
    Barge = asn_to_value(barge, Ni),
    TotalTime = asn_to_value(time, TotalTimeLen),
    Eik = asn_to_value(key,EikKey),
    Format = asn_to_value(format, FormatString),

    RecPara = #record_ctrl{ type = audio
                          , file_name = RecordFile
                          , format = Format
                          , max_time = TotalTime
                          , media = case Files of
                                        [] ->
                                            none;
                                        _ ->
                                             #media_ctrl{ type = audio
                                                        , filenames = Files
                                                        , times = 1}
                                    end
                          , barge = Barge
                          , term_key = Eik},
    MetaData = {rcs, RcsReq, TimerStart},
    {worker, {record_media, CallID, RecPara, MetaData}};
process_signal({viPa, ViPa}, RcsReq, CallID, TimerStart) ->
    info_manager:req(req_play_total),
    #'ViPa'{ file = FileList
           , it = PlayTimesIt
           , iv = IntervalTime
           , du = TotalTime
           , ni = Ni} = ViPa,
    Files = lists:foldr(fun(File, FilesIn) when is_record(File, 'AudioVideo') ->
                                Name = File#'AudioVideo'.videofile,
                                [Name|FilesIn];
                           (_, FilesIn) ->
                                FilesIn
                        end, [], tuple_to_list(FileList)),
    Total = asn_to_value(time, TotalTime),
    Interval = asn_to_value(mintime, IntervalTime),
    PlayTimes = asn_to_value(times, PlayTimesIt),
    Barge = asn_to_value(barge, Ni),
    PlayPara = #media_ctrl{ type = video
                          , filenames = Files
                          , times = PlayTimes
                          , interval = Interval
                          , max_time = Total
                          , barge = Barge},
    MetaData = {rcs, RcsReq, TimerStart},
    {worker, {play_media, CallID, PlayPara, MetaData}};
process_signal({viPc, ViPc}, RcsReq, CallID, TimerStart) ->
    info_manager:req(req_pc_total),
    #'ViPc'{ ni = Ni
           , ip = FileList
           , tp = AnPlayTimes
           , na = TimesRetry
           , fdt = FdtTime
           , idt = IdtTime
           , dp = DigitPattern
           , mn = MinDigit
           , mx = MaxDigit
           , rik = RikKey     %%重新开始收号
           , eik = EikKey     %%本次收号结束，不上报结束键
           %%, rsk = RskKey   %%当前整体放音收号重做，暂不支持
           %%, rtk = RtkKey
                    %%结束当前操作返回，
                    %%与Eik不同的是，单纯收号结束，
                    %%有可能是未打断继续放音的
                    %%暂时不支持，且当前收号结束时
                    %%也同时结束了放音
           , starttimer = StartFdtTimer} = ViPc,
    Barge = asn_to_value(barge, Ni),
    Fdt = asn_to_value(time, FdtTime),
    Idt = asn_to_value(time, IdtTime),
    info_manager:info("filelist ~p", [FileList]),
    Files = asn_to_value(filelist, FileList),
    Pattern = asn_to_value(pattern, DigitPattern),
    Min = asn_to_value(integer, MinDigit),
    Max = asn_to_value(integer, MaxDigit),
    Rik = asn_to_value(key, RikKey),
    Eik = asn_to_value(key, EikKey),
    Times = asn_to_value(times, TimesRetry),
    DCPara = #digit_ctrl{ timer = #digit_timer{ fdt = Fdt
                                              , idt = Idt
                                              , start_fdt_timer = StartFdtTimer}
                        , digit = case Pattern of
                                      none ->
                                          #digit_dgt{ min = Min
                                                    , max = Max
                                                    , rik = Rik
                                                    , eik = Eik};
                                      _ ->
                                          #digit_pattern{ pattern = Pattern}
                                  end
                        , media = case Files of
                                      [] ->
                                          none;
                                      _ ->
                                           #media_ctrl{ type = video
                                                      , filenames = Files
                                                      , times = AnPlayTimes}
                                  end
                        , times = Times
                        , barge = Barge},
    MetaData = {rcs, RcsReq, TimerStart},
    {worker, {digit_collect, CallID, DCPara, MetaData}};
process_signal({viPr, ViPr}, RcsReq, CallID, TimerStart) ->
    info_manager:req(req_pr_total),
    #'ViPr'{ ip = FileList
           , rf = RecordFileName
           , ni = Ni
           %%, prt = PreSpeechTimer
           %%, pst = PostSpeechTimer
           , rlt = TotalTimeLen
           , eik = EikKey
           , rcdfmt = FormatString
           , append = _Append} = ViPr,

    Files = asn_to_value(filelist, FileList),
    RecordFile = asn_to_value(string, RecordFileName),
    Barge = asn_to_value(barge, Ni),
    TotalTime = asn_to_value(time, TotalTimeLen),
    Eik = asn_to_value(key, EikKey),
    Format = asn_to_value(format, FormatString),

    RecPara = #record_ctrl{ type = video
                          , file_name = RecordFile
                          , format = Format
                          , max_time = TotalTime
                          , media = case Files of
                                        [] ->
                                            none;
                                        _ ->
                                            #media_ctrl{ type = video
                                                       , filenames = Files
                                                       , times = 1}
                                    end
                          , barge = Barge
                          , term_key = Eik},
    MetaData = {rcs, RcsReq, TimerStart},
    {worker, {record_media, CallID, RecPara, MetaData}};

process_signal({confPa, ConfPa}, RcsReq, _CallID, TimerStart) ->
    info_manager:req(req_conf_pa_total),
    #'ConfPa'{ roomID = RoomID
             , an = FileList
             , it = PlayTimesIt
             , iv = IntervalTime
             , du = TotalTime
             , ni = Ni} = ConfPa,
    Files = lists:foldr(fun(Name, FilesIn) when Name =:= 'An'; Name =:= asn1_NOVALUE ->
                                FilesIn;
                           (Name, FilesIn) ->
                                [Name|FilesIn]
                        end, [], tuple_to_list(FileList)),
    Total = asn_to_value(time, TotalTime),
    Interval = asn_to_value(mintime, IntervalTime),
    PlayTimes = asn_to_value(times, PlayTimesIt),
    Barge = asn_to_value(barge, Ni),
    PlayPara = #media_ctrl{ type = audio
                          , filenames = Files
                          , times = PlayTimes
                          , interval = Interval
                          , max_time = Total
                          , barge = Barge},
    MetaData = {rcs, RcsReq, TimerStart},
    {conf, {conf_play, RoomID, PlayPara, MetaData}};
process_signal({confPc, ConfPc}, RcsReq, _CallID, TimerStart) ->
    info_manager:req(req_conf_pc_total),
    #'ConfPc'{ roomID = RoomID
             , ip = FileList
             , mx = MaxDigit
             , mn = MinDigit
             , fdt = FdtTime
             , idt = IdtTime
             , na = TimesRetry
             , dp = DigitPattern
             } = ConfPc,
    Files = asn_to_value(filelist, FileList),
    Max = asn_to_value(integer, MaxDigit),
    Min = asn_to_value(integer, MinDigit),
    Fdt = asn_to_value(time, FdtTime),
    Idt = asn_to_value(time, IdtTime),
    Times = asn_to_value(times, TimesRetry),
    Pattern = asn_to_value(pattern, DigitPattern),

    DCPara = #digit_ctrl{ timer = #digit_timer{ fdt = Fdt
                                              , idt = Idt}
                        , digit = case Pattern of
                                      none ->
                                          #digit_dgt{ min = Min
                                                    , max = Max};
                                      _ ->
                                          #digit_pattern{ pattern = Pattern}
                                  end
                        , media = case Files of
                                      [] ->
                                          none;
                                      _ ->
                                           #media_ctrl{ type = audio
                                                      , filenames = Files}
                                  end
                        , times = Times},
    MetaData = {rcs, RcsReq, TimerStart},
    {conf, {conf_digit_collect, RoomID, DCPara, MetaData}};
process_signal({confPr, ConfPr}, RcsReq, _CallID, TimerStart) ->
    info_manager:req(req_conf_rec_total),
    #'ConfPr'{ roomID = RoomID
             , ip = FileList
             , rf = RecordFileName
             , eik = EikKey
             %%, ni = Ni
             %%, prt = PreSpeechTimer
             %%, pst = PostSpeechTimer
             , rcdfmt = FormatString
             , rlt = TotalTimeLen} = ConfPr,

    Files = asn_to_value(filelist, FileList),
    RecordFile = asn_to_value(string, RecordFileName),
    %%Barge = asn_to_value(barge, Ni),
    TotalTime = asn_to_value(time, TotalTimeLen),
    Format = asn_to_value(format, FormatString),
    Eik = asn_to_value(key, EikKey),

    RecPara = #record_ctrl{ file_name = RecordFile
                          , format = Format
                          , max_time = TotalTime
                          , media = case Files of
                                        [] ->
                                            none;
                                        _ ->
                                             #media_ctrl{ type = audio
                                                        , filenames = Files
                                                        , times = 1}
                                    end
                          , term_key = Eik},
    MetaData = {rcs, RcsReq, TimerStart},
    {conf, {conf_record_media, RoomID, RecPara, MetaData}};
process_signal(Signal, _RcsReq, _CallID, _TimerStart) ->
    error_logger:info_msg("bad ircp signal ~p",[Signal]),
    {unknown, drop}.

ircp_gen(Command, RcsReq) ->
    RcsReq#'RcsRpsMessage'{command = Command}.

ircp_send(Socket, Ircp, Seq, Dest, Src) ->
    %%info_manager:info("sending ircp: ~p", [Ircp]),
    case 'RcsRpsMsg':encode('RcsRpsMessage', Ircp) of
        {ok, BinIrcp} ->
            BinDest = list_to_binary(tuple_to_list(Dest)),
            BinSrc = list_to_binary(tuple_to_list(Src)),
            comm_send(Socket, ?TYPE_IRCP, ?CODE_IRCP, BinDest, BinSrc, Seq, BinIrcp);
        {error, Reason} ->
            info_manager:info("ircp encode error: ~p", [Reason])
    end.
comm_send(Socket, Type, Code, Dest, Src, Seq, Msg) ->
    Comm = comm_encode(Type, Code, Dest, Src, Seq, Msg),
    gen_tcp:send(Socket, Comm).

asn_to_value(filelist, asn1_NOVALUE) ->
    [];
asn_to_value(filelist, FileList) ->
    lists:foldr(fun(File, Acc) when is_list(File) ->
                        [File|Acc];
                   ({'AudioVideoFile', File, _,_,_}, Acc) when is_list(File) ->
                        [File|Acc];
                   (_, Acc) ->
                        Acc
                end, [], tuple_to_list(FileList));
asn_to_value(key, asn1_NOVALUE) ->
    none;
asn_to_value(key, []) ->
    none;
asn_to_value(key, [Key|_]) ->
    Key;
asn_to_value(barge, true) ->
    false;
asn_to_value(barge, _) ->
    true;
asn_to_value(time, 0) ->
    infinity;
asn_to_value(time, Time) when is_integer(Time) ->
    Time*1000;
asn_to_value(time, _) ->
    infinity;
asn_to_value(mintime, Time) when is_integer(Time) ->
    Time*1000;
asn_to_value(mintime, _) ->
    0;
asn_to_value(times, N) when is_integer(N) ->
    N;
asn_to_value(times, asn1_NOVALUE) ->
    0;
asn_to_value(integer, asn1_NOVALUE) ->
    none;
asn_to_value(integer, N) when is_integer(N) ->
    N;
asn_to_value(pattern, asn1_NOVALUE) ->
    none;
asn_to_value(pattern, Pattern) when is_list(Pattern) ->
    Pattern;
asn_to_value(format, asn_to_value) ->
    none;
asn_to_value(format, FormatString) ->
    lists:map(fun(F) -> case F of 
                            "g711u" ->
                                ?AUDIO_ULAW;
                            "g711a" ->
                                ?AUDIO_ALAW;
                            "pcm" ->
                                ?AUDIO_PCM8K16;
                            "evs" ->
                                ?AUDIO_EVS;
                            "amrnb" ->
                                ?AUDIO_AMRNB;
                            "amrwb" ->
                                ?AUDIO_AMRWB;
                            "h264" ->
                                ?VIDEO_H264;
                            _ ->
                                ?AUDIO_ULAW
                        end
              end, string:tokens(string:to_lower(FormatString), ":"));
asn_to_value(string, asn1_NOVALUE) ->
    none;
asn_to_value(string, Str) ->
    Str.

%%sdp port为0，代表未来可能启用，所以实际上需要预留一个资源，但并不启用，
%%概念上和port不为0,但a=inactive意义一致，但后者会收到rtcp，前者sdp返回
%%0,也就不会收到任何数据。
media_need_port(#media_descript{port = 0}) -> 1;
%%codeclist = [] 代表未启用，invite no sdp时可以用来区分资源是否启用
media_need_port(#media_descript{codeclist = []}) -> 0;
%%invite no sdp时，port =:= none，但还是要资源的
media_need_port(#media_descript{codeclist = _}) -> 1.

codec_trans(ulaw) ->
    ?AUDIO_ULAW;
codec_trans(alaw) ->
    ?AUDIO_ALAW;
%%codec_trans(g723) ->
%%    ?AUDIO_G723;
codec_trans(g729) ->
    ?AUDIO_G729;
codec_trans(amrnb) ->
    ?AUDIO_AMRNB;
codec_trans(amrwb) ->
    ?AUDIO_AMRWB;
codec_trans(evs) ->
    ?AUDIO_EVS;
codec_trans(h263) ->
    ?VIDEO_H263;
codec_trans(h264) ->
    ?VIDEO_H264;
codec_trans(_Unknown) ->
    unsupport.

trans_audio(#media_descript{port = Port, codeclist = [Codec|_]} = Desc) when Port =/= 0 ->
    Digit = trans_digit(Desc),
    case Codec of
        #codec{type = Type, payload = Payload, ext = Ext} when Type =:= amrnb
                                                             ; Type =:= amrwb ->
            #ext{ align = Align, modeset = ModeSet} = Ext,
            OctAlign = case Align of
                           eff ->
                               false;
                           oct ->
                               true;
                           {_, {default, eff}} ->
                               false;
                           {_, {default, oct}} ->
                               true
                       end,
            Mode = case ModeSet of
                           _ when is_list(ModeSet) ->
                               lists:max(ModeSet);
                           {_, {default, DefaultSet}} ->
                               lists:max(DefaultSet)
                       end,
            #audio_desc{ port = Port
                       , codec = codec_trans(Type)
                       , payload = Payload
                       , amr_ext = {Mode, OctAlign}
                       , digit = Digit};
        #codec{type = Type, payload = Payload, ext = Ext} when Type =:= evs ->
            #ext{mode_switch = MS, bitrate = BitRate} = Ext,
            ModeSwitch = case MS of
                0 -> primary;
                _ -> io
            end,
            #audio_desc{ port = Port
                       , codec = codec_trans(Type)
                       , payload = Payload
                       , evs_ext = {ModeSwitch, trans_bitrate(BitRate)}
                       , digit = Digit};
        #codec{type = Type, payload = Payload} ->
            #audio_desc{port = Port
                       ,codec=case codec_trans(Type) of
                                  unsupport ->
                                      info_manager:log_error("Sdp error",io_lib:format("audio codec ~p not supported",[Type])),
                                      info_manager:info("sdp audio codec ~p not support~n", [Type]),
                                      unsupport;
                                  Value ->
                                      Value 
                              end
                       ,payload = Payload
                       ,digit = Digit}
    end;
trans_audio(#media_descript{}) ->
    none.

trans_digit(#media_descript{digit = []}) ->
    #digit_desc{type = dtmf};
trans_digit(#media_descript{digit = [Digit|_]}) ->
    #digit_desc{ type = rfc2833
               , payload = Digit#codec.payload}.

trans_video(#media_descript{port = Port, codeclist = [Codec|_]}) when Port =/= 0 ->
    case Codec of
        #codec{type = Type, payload = Payload, ext = Ext} when Type =:= h264 ->
            #video_desc{ port = Port
                       , codec = codec_trans(Type)
                       , payload = Payload
                       , framerate = Ext#ext.framerate};
        #codec{type = Type, payload = Payload} ->
            #video_desc{port = Port
                       ,codec=case codec_trans(Type) of
                                  unsupport ->
                                      info_manager:log_error("Sdp error",io_lib:format("video codec ~p not supported",[Type])),
                                      info_manager:info("sdp video codec ~p not support~n", [Type]),
                                      unsupport;
                                  Value ->
                                      Value 
                              end
                       ,payload = Payload}
    end;
trans_video(#media_descript{}) ->
    none.

report_req_conf_stop(stop_confpa_media) ->
    info_manager:req(req_conf_stop_pa_succ);
report_req_conf_stop(stop_confpr_media) ->
    info_manager:req(req_conf_stop_rec_succ);
report_req_conf_stop(stop_conf_dc) ->
    info_manager:req(req_conf_stop_pc_succ).

trans_bitrate("5.9") -> 0;
trans_bitrate("7.2") -> 1;
trans_bitrate("8") -> 2;
trans_bitrate("9.6") -> 3;
trans_bitrate("13.2") -> 4;
trans_bitrate("16.4") -> 5;
trans_bitrate("24.4") -> 6;
trans_bitrate("32") -> 7;
trans_bitrate("48") -> 8;
trans_bitrate("64") -> 9;
trans_bitrate("96") -> 10;
trans_bitrate("128") -> 11.
