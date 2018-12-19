%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 25 Oct 2013 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(data_transfer).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_service/2, stop_service/2, cast/2, set/2]).

-define(SERVER, ?MODULE).

-define(EVS_MODE, 5).

-include("wave.hrl").
-include("rtp.hrl").
-include("audio_core.hrl").
-include("video_core.hrl").
-include("media_desc.hrl").
-include("iso_base_file.hrl").

-record(state, { socket
               , dest = none
               , source
               , media_sender
               , listeners = sets:new()

               , timestamp = 1600  %%timestamp 应该用随机值吗
               , ssrc
               , times
               , interval
               , silence = 0

               , data = <<>>
               , amr_estate
               , amr_dstate
               , pos = 0

               , evs_encoder
               , evs_decoder

               , dc_dsp
               , last_digit
               , last_seq = -1

               , video_seq = 1
               , video_circle_count = 0
               , video_circle_index = 2
               , video_circle = {2,2,3}
               , video_delay = 6000

               , audio_desc = none
               , video_desc = none

               , client_addr = {{0,0,0,0}, 0}
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
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    process_flag(trap_exit, true),
    clear_rtp(),
    %%RTP统计15分钟上报网管一次，在放音过程中可以尽量少的统计并不影响统计结果
    %%现网放音基本上10秒左右，以一次放音统计两次计，取5秒一次比较合适
    timer:send_interval(timer:seconds(5), rtp_report),
    Dsp = dtmf:init_dsp(),
    info_manager:info("~p ~p init",[?MODULE, self()]),
    {ok, #state{dc_dsp = Dsp}}.

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

handle_call({set, {audio_desc, AudioDesc}}, _From, State) ->
    info_manager:info("audio desc ~p",[AudioDesc]),
    {reply, ok, State#state{audio_desc = AudioDesc}};
handle_call({set, {rtp, RtpInfo}}, _From, State) ->
    {Socket, AddrPort, SSRC, Desc} = RtpInfo,
    info_manager:info("client ~p new addr ~p whth desc ~p",[Socket, AddrPort, Desc]),
    erlang:port_connect(Socket, self()),
    unlink(Socket),
    case Desc of
        AudioDesc when is_record(AudioDesc, audio_desc) ->
            case AudioDesc#audio_desc.codec  of
                Amr when Amr =:= ?AUDIO_AMRNB
                       ; Amr =:= ?AUDIO_AMRWB ->
                    info_manager:info("client mediatype is ~p",[Amr]),
                    {reply, ok ,State#state{ socket = Socket
                                           , dest = client
                                           , client_addr = AddrPort
                                           , amr_estate = amr_codec:init(Amr, encode)
                                           , amr_dstate = amr_codec:init(Amr, decode)
                                           , ssrc = SSRC
                                           , audio_desc = AudioDesc}};
                Evs when Evs =:= ?AUDIO_EVS ->
                    info_manager:info("client mediatype is ~p",[Evs]),
                    #audio_desc{ evs_ext = {primary, EvsMode}} = AudioDesc,
                    Encoder = evs_port:new_enocder(audio_core:evs_primary_bits(?AUDIO_EVS, EvsMode)),
                    Decoder = evs_port:new_decoder(),
                    {reply, ok ,State#state{ socket = Socket
                                           , dest = client
                                           , client_addr = AddrPort
                                           , ssrc = SSRC
                                           , evs_encoder = Encoder
                                           , evs_decoder = Decoder
                                           , audio_desc = AudioDesc}};
                G7x when G7x =:= ?AUDIO_G729
                       ; G7x =:= ?AUDIO_ULAW
                       ; G7x =:= ?AUDIO_ALAW ->
                    info_manager:info("client mediatype is ~p",[G7x]),
                    {reply, ok, State#state{ socket = Socket
                                           , dest = client
                                           , client_addr = AddrPort
                                           , ssrc = SSRC
                                           , audio_desc = AudioDesc}}
            end;
        VideoDesc when is_record(VideoDesc, video_desc) ->
            Framerate = VideoDesc#video_desc.framerate,
            info_manager:info("video fps is ~p",[Framerate]),
            case video_circle(Framerate) of
                {error, F} ->
                    info_manager:log_error("Set rtp error", io_lib:format("framerate ~p not supported", [F])),
                    {reply, error, State};
                {Circle, Delay} ->
                    {reply, ok, State#state{ socket = Socket
                                           , dest = client
                                           , client_addr = AddrPort
                                           , ssrc = SSRC
                                           , video_desc = VideoDesc
                                           , video_circle = Circle
                                           , video_delay = Delay}}
            end
    end;

handle_call({start, {send, _, _, {Data, _, Times}}}, From, State) when Data =:= <<>>
                                                                     ; Times =< 0 ->
    {Sender, _} = From,
    media_play:cast(Sender, send_over),
    {reply, ok, State};

handle_call({start, {send, Dest, Source, {Data, Interval, Times}}}, From, State) ->
    pulse_server:add_player(self()),
    {Sender, _} = From,
    link(Sender),
    case Data of
        _ when is_binary(Data) ->
            info_manager:info("data transfer get new play with data size ~p", [byte_size(Data)]);
        _ when is_list(Data) ->
            info_manager:info("data transfer get new play with list length ~p", [length(Data)])
    end,
    {reply, ok, State#state{ media_sender = Sender
                           , dest = Dest
                           , source = Source

                           , pos = 0
                           , interval = Interval
                           , times = Times - 1

                           , data = Data}};

handle_call({start, {listen, Type}}, From, #state{ listeners = Listeners
                                                 , socket = Socket} = State) ->
    case Socket of
        undefined ->
            ok;
        _ ->
            case sets:size(Listeners) of
                0 ->
                    info_manager:info("clean buffer~n"),
                    clean_buffer(Socket),
                    inet:setopts(Socket, [{active, true}]);
                _ -> ok
            end
    end,
    {Listener, _} = From,
    link(Listener),
    info_manager:info("new listeners ~p for ~p", [Listener, Type]),
    {reply, ok, State#state{listeners = sets:add_element( {Listener, Type}
                                                        , Listeners)}};

handle_call({stop, {listen, data}}, From, #state{listeners = Listeners} = State) ->
    {Pid, _} = From,
    info_manager:info("data_transfer ~p remove listener ~p", [self(), Pid]),
    NListeners = sets:filter(fun({L, _}) -> L =/= Pid end, Listeners),
    {reply, {ok}, State#state{listeners = NListeners}};

handle_call(Request, _From, State) ->
    info_manager:info("data_transfer unknow request: ~p", [Request]),
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

handle_cast(timer_pulse, #state{ socket = Socket
                               , media_sender = Sender
                               , client_addr = {Address, Port}

                               , ssrc = SSRC
                               , timestamp = Timestamp
                               , video_delay = Delay

                               , data = Data
                               , pos = Pos
                               , silence = Silence
                               , times = Times
                               , interval = Interval
                               , audio_desc = none
                               , video_circle_count = 0
                               , video_circle_index = Index
                               , video_circle = Circle
                               , video_desc = VideoDesc
                               , video_seq = Seq } = State) when Pos < length(Data) ->
    PayloadType = VideoDesc#video_desc.payload,
    case Silence of
        _ when Silence > 0 ->
            {Packet, NTimestamp} = gen_silence_packet(?VIDEO_H264, Timestamp, SSRC, PayloadType),
            data_send(Socket, Address, Port, Packet),
            {noreply, State#state{ silence = Silence - 1
                                 , timestamp = NTimestamp}};
        _ ->
            {Rtps, NPos, NSeq, Type, IsOver} = gen_packet_video(Data, Pos, Timestamp, Seq, SSRC, PayloadType),
            NTimestamp = Timestamp + Delay,
            lists:foreach(fun(S) ->
                Bin = rtp:encode(S),
                data_send(Socket, Address, Port, Bin) end, Rtps),
            case {IsOver, Times} of
                {false, _} when Type =:= ?NAL_UNIT_TYPE_SEI
                              ; Type =:= ?NAL_UNIT_TYPE_SPS
                              ; Type =:= ?NAL_UNIT_TYPE_PPS ->
                    handle_cast(timer_pulse, State#state{ pos = NPos
                                                        , video_seq = NSeq});
                {false, _} ->
                    NIndex = (Index+1) rem tuple_size(Circle),
                    NCount = element(NIndex + 1, Circle),
                    {noreply, State#state{ pos = NPos
                                         , video_seq = NSeq
                                         , video_circle_count = NCount
                                         , video_circle_index = NIndex
                                         , timestamp = NTimestamp}};
                {true, 0} ->
                    media_play:cast(Sender, send_over),
                    pulse_server:remove_player(self()),
                    {noreply, State#state{ pos = NPos
                                         , timestamp = NTimestamp}};
                {true, _} ->
                    {noreply, State#state{ times = Times - 1
                                         , pos = 0
                                         , video_seq = NSeq
                                         , video_circle_count = 0
                                         , video_circle_index = 2
                                         , timestamp = NTimestamp
                                         , silence = case Interval rem 20 of
                                                        0 -> Interval div 20;
                                                        _ -> Interval div 20 + 1
                                                     end}}
            end
    end;

handle_cast(timer_pulse, #state{ video_circle_count = Count
                               , pos = Pos
                               , data = Data
                               , audio_desc = none
                               } = State) when Pos < length(Data) ->
    %%info_manager:info("video count ~p, not send", [Count]),
    {noreply, State#state{video_circle_count = Count - 1}};

handle_cast(timer_pulse, #state{ dest = Dest
                               , source = media_play
                               , video_desc = none} = State) ->
    #state{ socket = Socket
          , client_addr = {Address, Port}
          , media_sender = Sender

          , ssrc = SSRC
          , timestamp = Timestamp
          , silence = Silence
          , pos = Pos
          , data = Data
          , times = Times
          , interval = Interval
          , audio_desc = AudioDesc
          } = State,
    #audio_desc{ codec = MediaType
               , payload = PayloadType
               , amr_ext = AmrExt} = AudioDesc,
    Align = case AmrExt of
        {_, A} ->
            A;
        _ ->
            false
    end,
    try
    {Frame, NPos, IsEnd, NSilence} = gen_audio_frame(MediaType, Data, Pos, Silence),
    NTimestamp = case Dest of
        client when Frame =/= <<>> ->
            {RTPPacket, NT} = frame_to_rtp(MediaType, Frame, Timestamp, SSRC, {PayloadType, Align}),
            data_send(Socket, Address, Port, RTPPacket),
            NT;
        Mixer when is_pid(Mixer), Frame =/= <<>> ->
            gen_server:cast(Mixer, {data, self(), {none, none}, Frame}),
            Timestamp;
        _ ->
            %% now amr video pcm not upadte timestamp
            Timestamp
    end,
    case {IsEnd, Times} of
        _ when Silence > 0 ->
            {noreply, State#state{ silence = NSilence
                                 , timestamp = NTimestamp}};
        {false, _} ->
            {noreply, State#state{ pos = NPos
                                 , timestamp = NTimestamp}};
        {true, 0} ->
            media_play:cast(Sender, send_over),
            pulse_server:remove_player(self()),
            {noreply, State#state{ pos = NPos
                                 , timestamp = NTimestamp}};
        {true, _} ->
             {noreply, State#state{ times = Times - 1
                                  , pos = 0
                                  , timestamp = NTimestamp
                                  , silence = case Interval rem 20 of
                                                 0 -> Interval div 20;
                                                 _ -> Interval div 20 + 1
                                             end}}
    end
    catch
        Class:Reason ->
            media_play:cast(Sender, send_over),
            pulse_server:remove_player(self()),
            error_logger:info_msg("play error type: ~p,  pos: ~p, media: ~p ~n",[MediaType, Pos, Data]),
            info_manager:log_error("Media play error", io_lib:format("~p:~p", [Class, Reason])),
            {noreply, State}
   end;

handle_cast({none, mixer, Data}, #state{ socket = Socket
                                       , client_addr = {Address, Port}
                                       , dest = client

                                       , ssrc = SSRC
                                       , amr_estate =EState
                                       , audio_desc = AudioDesc
                                       , timestamp = Timestamp} = State) when is_binary(Data) ->
    PayloadType = AudioDesc#audio_desc.payload,
    MediaType = AudioDesc#audio_desc.codec,
    case MediaType of
        AMR when AMR =:= ?AUDIO_AMRNB
               ; AMR =:= ?AUDIO_AMRWB ->
            {AmrMode, Align} = AudioDesc#audio_desc.amr_ext,
            NData = case MediaType of
                ?AUDIO_AMRNB ->
                    Data;
                ?AUDIO_AMRWB ->
                    audio_core:convert({Data, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []})
            end,
            {NEState, Frame} = amr_codec:encode_frame(MediaType, EState, NData, AmrMode),
            {Packet, NTimestamp} = frame_to_rtp(MediaType, Frame, Timestamp, SSRC, {PayloadType, Align}),
            data_send(Socket, Address, Port, Packet),
            {noreply, State#state{ timestamp = NTimestamp
                                 , amr_estate = NEState}};
        ?AUDIO_EVS ->
            EvsEncoder = State#state.evs_encoder,
            Frame = evs_port:encode_frame(EvsEncoder, Data),
            {Packet, NTimestamp} = frame_to_rtp(MediaType, Frame, Timestamp, SSRC, {PayloadType, none}),
            data_send(Socket, Address, Port, Packet),
            {noreply, State#state{ timestamp = NTimestamp}};
        T when T =:= ?AUDIO_ULAW
             ; T =:= ?AUDIO_ALAW ->
            FrameData = audio_core:convert({Data, ?AUDIO_PCM8K16}, {MediaType, []}),
            {Packet, NTimestamp} = frame_to_rtp(MediaType, FrameData, Timestamp, SSRC, {PayloadType, none}),
            data_send(Socket, Address, Port, Packet),
            {noreply, State#state{timestamp = NTimestamp}}
    end;

handle_cast({none, mixer, Info}, State) ->
    case Info of
        Data when is_binary(Info) ->
            %%现在只有会议录音使用，此处的编码是AUDIO_PCM8K16
            %%目前支持录制alaw,ulaw,pcm8k16,evs,amrnb,amrwb编码的文件
            DState = none,
            sets:fold(fun({Pid, {data, mixer, Format}}, DS) when Format =:= ?AUDIO_AMRNB
                                                               ; Format =:= ?AUDIO_AMRWB ->
                              gen_server:cast(Pid, {data, self(), {none, none}, Data}),
                              DS;
                         ({Pid, {data, mixer, Format}}, _DS) ->
                              {NData, NDS} = frame_convert({Data, ?AUDIO_PCM8K16}, {Format, DState}),
                              gen_server:cast(Pid, {data, self(), {none, none}, NData}),
                              NDS;
                         ({_,_}, DS) ->
                              DS
                          end, DState, State#state.listeners),
            ok;
        Digit when is_integer(Info) ->
            sets:fold(fun({Pid, {digit, mixer}}, _) ->
                              gen_server:cast(Pid, {digit, Digit});
                         ({_, _}, _) ->
                              ok
                          end, ok, State#state.listeners)
    end,
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
handle_info({udp, _Socket, Addr, Port, Packet}, #state{client_addr = {Addr, Port}} = State) ->
    update_rtp(rtp_recv, byte_size(Packet)),
    try
        case rtp:decode(Packet) of
            {ok, Rtp} ->
                case digit_or_data(Rtp, State) of
                    {digit, _, none, NDsp} ->
                        {noreply, State#state{dc_dsp = NDsp}};
                    {digit, Start, Digit, NDsp} when Start =:= true ->
                        info_manager:info("got ~c, send to listeners~n",[Digit]),
                        sets:fold(fun({Pid, {digit, client}}, _) ->
                                    gen_server:cast(Pid, {digit, Digit});
                                  ({_,_}, _) ->
                                    ok
                                  end, ok, State#state.listeners),
                        {noreply, State#state{ last_seq = Rtp#rtp.sequence_number
                                             , dc_dsp = NDsp
                                             , last_digit = Digit }};
                    {digit, _, _, NDsp} ->
                        {noreply, State#state{ last_seq = Rtp#rtp.sequence_number
                                             , dc_dsp = NDsp}};
                    {data, Data, NDsp} when is_record(State#state.video_desc, video_desc) ->
                        MediaType = (State#state.video_desc)#video_desc.codec,
                        sets:fold(fun({Pid, {data, client, Format}}, _) when Format =:= MediaType ->
                                         gen_server:cast(Pid, {data, self(), {Rtp#rtp.payload_type, Rtp#rtp.timestamp}, Data});
                                     ({_,_}, _) ->
                                         ok 
                                      end, ok, State#state.listeners),
                        {noreply, State#state{ last_seq = Rtp#rtp.sequence_number
                                             , dc_dsp = NDsp
                                             , last_digit = none}};
                    {data, Data, NDsp} when is_record(State#state.audio_desc, audio_desc)->
                        MediaType = (State#state.audio_desc)#audio_desc.codec,
                        Align = case MediaType of
                            _ when MediaType =:= ?AUDIO_AMRNB; MediaType =:= ?AUDIO_AMRWB ->
                                {_, OctetAlign} = (State#state.audio_desc)#audio_desc.amr_ext,
                                OctetAlign;
                            _ ->
                                none
                        end,
                        Frame = rtp_to_frame(Data, {MediaType, Align}),
                        DState = case MediaType of
                                 ?AUDIO_AMRNB -> State#state.amr_dstate;
                                 ?AUDIO_AMRWB -> State#state.amr_dstate;
                                 ?AUDIO_EVS -> State;
                                 _ -> none
                                 end,
                        %%当多人订阅，同时类型相同时，会多次计算。需要改进。但当前
                        %%只有录音和会议会少量用到，暂时实现成这样
                        %%大量转换刚才开个脑洞，感觉要在audio_core中抽象一种frame_convert
                        %%这种批量转码传递似乎还是有哪里不妥，在data_transfer和mixer
                        %%转码都不大合适，又不喜欢mixer_agent。
                        %%目前支持ulaw,alaw,evs,amrnb,amrwb到ulaw,alaw,pcm8k16,evs,amrnb,amrwb的转换
                        %%其中目标格式为amrnb或amrwb时，先转换为中间格式PCM8K16，待写入文件时再做PCM8K16到amrnb或amrwb的转换
                        NDState = sets:fold(fun({Pid, {data, client, Format}}, _DS) when Format =:= ?AUDIO_AMRNB
                                                                                       ; Format =:= ?AUDIO_AMRWB ->
                                                    {TargetFrame, NDS} = frame_convert({Frame, MediaType}, {?AUDIO_PCM8K16, DState}),
                                                    gen_server:cast(Pid, {data, self(), {Rtp#rtp.payload_type, Rtp#rtp.timestamp}, TargetFrame}),
                                                    NDS;
                                               ({Pid, {data, client, Format}}, _DS) ->
                                                    {TargetFrame, NDS} = frame_convert({Frame, MediaType}, {Format, DState}),
                                                    gen_server:cast(Pid, {data, self(), {Rtp#rtp.payload_type, Rtp#rtp.timestamp}, TargetFrame}),
                                                    NDS;
                                               ({_,_}, DS) ->
                                                    DS
                                                end, DState, State#state.listeners),
                        {noreply, State#state{ amr_dstate = NDState
                                             , last_seq = Rtp#rtp.sequence_number
                                             , dc_dsp = NDsp
                                             , last_digit = none}};
                    {comfort_noise, _} ->
                        {noreply, State#state{ last_seq = Rtp#rtp.sequence_number
                                             , last_digit = none}};
                    {badpayload, _} ->
                        {noreply, State#state{ last_seq = Rtp#rtp.sequence_number
                                             , last_digit = none}};
                    {none} ->
                        {noreply, State}
                end;
            _ ->
                {noreply, State}
        end
    catch
        _:_ ->
            %%rtp解析失败，丢弃
            %%20ms写入日志的频率太高，目前在收到错误包时不输出任何错误日志并且资源正常接受请求
            %%如果遇到问题打开日志DEBUG
            %%info_manager:log_debug("decode rtp error", io_lib:format("~p:~p", [Class, Reason])),
            {noreply, State}
    end;
%%handle_info({udp, _Socket, Addr, Port, _Packet}, State) ->
%%    io:format("get data from ~p:~p, not match~n",[Addr, Port]),
%%    {noreply, State};

handle_info(rtp_report, #state{client_addr = {Address, _}} = State) ->
    report_rtp(Address),
    clear_rtp(),
    {noreply, State};

handle_info({'EXIT', Pid, _}, #state{media_sender = Pid} = State) ->
    info_manager:info("data transfer get media sender exit"),
    pulse_server:remove_player(self()),
    {noreply, State};

handle_info( {'EXIT', Pid, _} , #state{ listeners = Listeners
                                      , socket = Socket} = State) when Socket =/= undefined ->
    info_manager:info("data transfer get listener ~p exit",[Pid]),
    NListeners = sets:filter(fun({L, _}) -> L =/= Pid end, Listeners),
    case sets:size(NListeners) of
        0 -> inet:setopts(Socket, [{active, false}]);
        _ -> ok
    end,
    {noreply, State#state{listeners = NListeners}};
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
terminate(_Reason, #state{client_addr = {Address, _}} = State) ->
    catch port_close(State#state.evs_encoder),
    catch port_close(State#state.evs_decoder),
    report_rtp(Address).

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

set(Pid, Msg) ->
    gen_server:call(Pid, {set, Msg}, 500).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

start_service(Pid, Service) ->
    gen_server:call(Pid, {start, Service}).

stop_service(Pid, Service) ->
    gen_server:call(Pid, {stop, Service}, 1000).
%%不再需要stop service了，现行架构是如果控制进程退出，服务自动停止，减
%%很多不必要的代码

%%%===================================================================
%%% Internal functions
%%%===================================================================

gen_silence_frame(?AUDIO_ULAW) -> binary:copy(<<16#FF>>, 160);
gen_silence_frame(?AUDIO_ALAW) -> binary:copy(<<16#D5>>, 160);
gen_silence_frame(?AUDIO_AMRNB) -> <<>>;
gen_silence_frame(?AUDIO_AMRWB) -> <<>>;
gen_silence_frame(?AUDIO_EVS) -> <<>>;
gen_silence_frame(?AUDIO_G729) -> <<>>;
%%gen_silence_frame(?AUDIO_PCM8K16) -> binary:copy(<<16#0000>>, 160);
gen_silence_frame(?AUDIO_PCM8K16) -> <<>>;
gen_silence_frame(?VIDEO_H264) -> <<>>.

gen_audio_frame(MediaType, _Data, Pos, Silence) when Silence > 0 ->
    {gen_silence_frame(MediaType), Pos, false, Silence - 1};

gen_audio_frame(MediaType, Data, Pos, Silence) ->
    FrameBytes = audio_core:frame_bytes(Data, MediaType),
    case  Pos + FrameBytes of
        NPos when NPos < byte_size(Data) ->
            Frame = binary_part(Data, Pos, FrameBytes),
            {Frame, NPos, false, Silence};
        NPos when NPos =:= byte_size(Data) ->
            Frame = binary_part(Data, Pos, FrameBytes),
            {Frame, NPos, true, Silence};
        %% PCM，ULAW 和 ALAW 可能存在不足一帧的数据，但amr不会
        _ when MediaType =:= ?AUDIO_ULAW
             ; MediaType =:= ?AUDIO_ALAW
             ; MediaType =:= ?AUDIO_PCM8K16 ->
            Frame = binary_part(Data, Pos, byte_size(Data) - Pos),
            {Frame, byte_size(Data), true, Silence};
        _ ->
            {<<>>, byte_size(Data), true, Silence}
    end.

gen_packet_video(Data, Pos, Timestamp, Seq, SSRC, PayloadType) ->
    Nal = lists:nth(Pos + 1, Data),
    <<0:1/unsigned, _NRI:2/unsigned, Type:5/unsigned, _Remain/binary>> = Nal,
    Max = 1388, %%封包长度，依网络不同而变，后续添加配置或自动获取
    Rtps = nal2rtp(Nal, Max, start, PayloadType, Seq, Timestamp, SSRC, []),
    NSeq = length(Rtps) + Seq,
    case Pos + 1 of
        NPos when NPos >= length(Data) ->
            {Rtps, NPos, NSeq, Type, true};
        NPos ->
            {Rtps, NPos, NSeq, Type, false}
    end.

gen_silence_packet(MediaType, Timestamp, SSRC, PayloadInfo) ->
    case gen_silence_frame(MediaType) of
        <<>> ->
            {<<>>, Timestamp};
        SilenceFrame ->
            Packet = frame_to_rtp(MediaType, SilenceFrame, Timestamp, SSRC, PayloadInfo),
            {Packet, next_timestamp(Timestamp, MediaType)}
    end.

genrtp(audio, P, Seq, Time, SSRC, PT) ->
    #rtp{payload_type = PT,
         sequence_number = Seq,
         timestamp = Time,
         ssrc = SSRC,
         payload = P};
genrtp(video, P, Seq, Time, SSRC, _) ->
    #rtp{payload_type = 34,
         marker = 1,
         %%sequence_number = (Time div (90000 div 15)) + 1,
         sequence_number = Seq,
         timestamp = Time,
         ssrc = SSRC,
         payload = P}.

clean_buffer(Socket) ->
    case gen_udp:recv(Socket, 65536, 0) of
       {error, timeout} ->
           ok;
       {error, Reason} ->
           exit(Reason);
       _ ->
           clean_buffer(Socket)
    end.

digit_or_data( #rtp{ payload_type = Type
                   , sequence_number = Seq} = Rtp
             , #state{ audio_desc = AudioDesc
                     , dc_dsp = Dsp
                     , last_seq = LastSeq}) when AudioDesc#audio_desc.digit#digit_desc.type =:= rfc2833
                                               , AudioDesc#audio_desc.digit#digit_desc.payload =:= Type
                                               , byte_size(Rtp#rtp.payload) =:= 4 ->

    {ok, DTMF} = rtp:decode_dtmf(Rtp#rtp.payload),
    Marker = case Rtp#rtp.marker of
        1 when LastSeq < Seq -> true;
        _ -> false
    end,
    {digit, Marker, event_to_char(DTMF#dtmf.event), Dsp};
digit_or_data(#rtp{payload_type = Type} = Rtp, #state{audio_desc = AudioDesc, dc_dsp = Dsp} = State)
                    when AudioDesc#audio_desc.digit#digit_desc.type =:= dtmf
                       , AudioDesc#audio_desc.payload =:= Type
                       , (Type =:= 8) or (Type =:= 0) ->
    Data = Rtp#rtp.payload,
    Trans = case Type of
        8 ->
            ?AUDIO_ALAW;
        0 ->
            ?AUDIO_ULAW
    end,
    case byte_size(Data) of
        160 ->
           PCM_Bin = audio_core:convert({Data, Trans}, {?AUDIO_PCM8K16, []}),
           {NDsp, Digit} = dtmf:parse(Dsp, PCM_Bin),
            case Digit of
                0 ->
                    {data, Data, NDsp};
                E when E =/= State#state.last_digit ->
                    %%暂时均按初值上报
                    {digit, true, char_filter(E), NDsp};
                E ->
                    {digit, false, char_filter(E), NDsp}
            end;
        _ ->
            {data, Data, Dsp}
    end;
digit_or_data(#rtp{payload_type = Type, payload = Data}, #state{audio_desc = AudioDesc, dc_dsp = Dsp})
                                        when AudioDesc#audio_desc.payload =:= Type ->
    {data, Data, Dsp};
digit_or_data(#rtp{payload_type = Type, payload = Data}, #state{video_desc = VideoDesc, dc_dsp = Dsp})
                                        when VideoDesc#video_desc.payload =:= Type ->
    {data, Data, Dsp};
digit_or_data(#rtp{payload_type = ?RTP_PAYLOAD_CN, payload = Data}, _) ->
    {comfort_noise, Data};
%%一般是rtp payload和audio_desc payload不符
digit_or_data(#rtp{payload = Data}, _) ->
    {badpayload, Data};
digit_or_data(_RTP, _) ->
    {none}.

%% rfc2833 3.10
event_to_char(none) -> none;
event_to_char(11) -> $#; % 常用的靠前来加速
event_to_char(10) -> $*; % 常用的靠前来加速
event_to_char(N) when 0 =< N, N =< 9 -> $0 + N;
%%暂时不支持这种东西，但是留个位
%%event_to_char(16) -> flash;
%%event_to_char(12) -> $A;
%%event_to_char(13) -> $B;
%%event_to_char(14) -> $C;
%%event_to_char(15) -> $D;
event_to_char(_) -> none.

char_filter($*) -> $*;
char_filter($#) -> $#;
char_filter(N) when $0 =< N, N =< $9 -> N;
char_filter(_) -> none.

frame_to_rtp(MediaType, Frame, Timestamp, SSRC, {PayloadType, Align})
        when MediaType =:= ?AUDIO_AMRNB
           ; MediaType =:= ?AUDIO_AMRWB ->
    FT = amr_codec:frame_mode(Frame),
    BitLen = amr_codec:frame_bits(MediaType, FT),
    <<_:8, AMR:BitLen/bitstring, _/bitstring>> = Frame,
    CMR = 15,
    Piece = case Align of
        true ->
            pad_zero(<<CMR:4/unsigned, 0:4, 0:1, FT:4/unsigned, 1:1, 0:2, AMR/bitstring>>);
        false ->
            pad_zero(<<CMR:4/unsigned, 0:1, FT:4/unsigned, 1:1, AMR/bitstring>>)
    end,
    RtpStruct = genrtp( audio
                      , Piece
                      , (Timestamp div packet_time(MediaType)) + 1
                      , Timestamp
                      , SSRC
                      , PayloadType),
    {rtp:encode(RtpStruct), next_timestamp(Timestamp, MediaType)};

frame_to_rtp(MediaType, Frame, Timestamp, SSRC, {PayloadType, _Align})
        when MediaType =:= ?AUDIO_EVS->
    <<_TOC:1/binary, Piece/binary>> = Frame,
    RtpStruct = genrtp( audio
                      , Piece
                      , (Timestamp div packet_time(MediaType)) + 1
                      , Timestamp
                      , SSRC
                      , PayloadType),
    {rtp:encode(RtpStruct), next_timestamp(Timestamp, MediaType)};
frame_to_rtp(MediaType, Frame, Timestamp, SSRC, {PayloadType, _})
        when MediaType =:= ?AUDIO_ULAW
           ; MediaType =:= ?AUDIO_ALAW
           ; MediaType =:= ?AUDIO_G729 ->
    RtpStruct = genrtp( audio
                      , Frame
                      , (Timestamp div packet_time(MediaType)) + 1
                      , Timestamp
                      , SSRC
                      , PayloadType),
    {rtp:encode(RtpStruct), next_timestamp(Timestamp, MediaType)}.

rtp_to_frame(Payload, {MediaType, Align}) when MediaType =:= ?AUDIO_AMRNB
                                             ; MediaType =:= ?AUDIO_AMRWB ->
    {Mode, AMRP, _ModeExp} = case Align of
        false ->
            <<CMR:4/unsigned, _:1, FT:4/unsigned, _:1, AMR/bitstring>> = Payload,
            {FT, AMR, CMR};
        true ->
            <<CMR:4/unsigned, _:4, _:1, FT:4/unsigned, _:1, _:2, AMR/bitstring>> = Payload,
            {FT, AMR, CMR}
    end,
    AMRBitLen = amr_codec:frame_bits(MediaType, Mode),
    <<AMR_REAL:AMRBitLen/bitstring, _/bitstring>> = AMRP,
    pad_zero(<<0:1, Mode:4/unsigned, 1:1, 0:2, AMR_REAL/bitstring>>);
rtp_to_frame(Payload, {MediaType, _}) when MediaType =:= ?AUDIO_EVS ->
    Payload;
rtp_to_frame(Payload, {MediaType, _}) when MediaType =:= ?AUDIO_ULAW
                                         ; MediaType =:= ?AUDIO_ALAW ->
    Payload.

packet_time(?AUDIO_AMRWB) -> 320;
packet_time(?AUDIO_EVS) -> 320;
packet_time(?AUDIO_ULAW)  -> 160;
packet_time(?AUDIO_ALAW)  -> 160;
packet_time(?AUDIO_G729)  -> 160;
packet_time(?AUDIO_AMRNB) -> 160.

next_timestamp(Timestamp, MediaType) ->
    (Timestamp + packet_time(MediaType)) band 16#FFFFFFFF.

pad_zero(Bits) when byte_size(Bits)*8 =:= bit_size(Bits) ->
    Bits;
pad_zero(Bits) ->
    PLen = byte_size(Bits)*8 - bit_size(Bits),
    <<Bits/bitstring, 0:PLen>>.

%% 网络最大传输单元是1500字节,除去UDP头、RTP头等,最多用于
%% 传输有效音视频数据的大小是1388字节;当一个视频帧
%% (access unit)的长度大于1388字节时需要分片，一个帧的所有
%% 分片只有最后一个分片Marker置1，表示是最后一个分片，一个
%% 有效视频帧结束;其余分片Marker置0, 辅助帧(SEI)、序列参数
%% 集帧(SPS)、图像参数集帧(PPS)不属于视频帧,目前经过对
%% ffmpeg的测试，发现SEI帧最大的长度是1148字节(小于1388字节)，
%% 因此不需要分片;对于SPS、PPS两个帧,通过测试其它板卡,发现长
%% 度也较小，目前是按照两个帧长都小于1388字节实现的,不做分片
%% 处理；如果后续发现这两帧长度大于1388字节，需要再调整
%% SEI、SPS、PPS帧不包含实际视频图像，参照其它板卡发包规则,
%% 目前三种帧发包开始时一次性发出,此外，三帧所封装到的RTP包
%% 的RTP头中marker字段统一置0;

nal2rtp(Nal, Max, start, PayloadType, Seq, Time, SSRC,_) when byte_size(Nal) =< Max ->
    <<0:1/unsigned, _NRI:2/unsigned, Type:5/unsigned, _Remain/binary>> = Nal,
    Marker = case Type of
        _ when Type =:= ?NAL_UNIT_TYPE_SEI
             ; Type =:= ?NAL_UNIT_TYPE_SPS
             ; Type =:= ?NAL_UNIT_TYPE_PPS ->
            0;
        _ ->
            1
    end,
    [#rtp{payload_type = PayloadType,
         marker = Marker,
         sequence_number = Seq,
         timestamp = Time,
         ssrc = SSRC,
         payload = Nal}];
nal2rtp(Nal, Max, _, PayloadType, Seq, Time, SSRC, [NRI, Type]) when byte_size(Nal) =< (Max-2) ->
    FUI = <<0:1/unsigned, NRI:2/unsigned, 28:5/unsigned>>,
    FUH = <<0:1/unsigned,1:1/unsigned,0:1/unsigned, Type:5/unsigned>>,
    [#rtp{payload_type = PayloadType,
         marker = 1,
         sequence_number = Seq,
         timestamp = Time,
         ssrc = SSRC,
         payload = <<FUI/binary, FUH/binary, Nal/binary>>}];
nal2rtp(Nal, Max, nonstart, PayloadType, Seq, Time, SSRC, [NRI, Type]) ->
    FUI = <<0:1/unsigned, NRI:2/unsigned, 28:5/unsigned>>,
    FUH = <<0:1/unsigned,0:1/unsigned,0:1/unsigned, Type:5/unsigned>>,
    Len = Max - 2,
    <<Piece:Len/binary, Next/binary>> = Nal,
    [#rtp{payload_type = PayloadType,
         marker = 0,
         sequence_number = Seq,
         timestamp = Time,
         ssrc = SSRC,
         payload = <<FUI/binary, FUH/binary, Piece/binary>>}|nal2rtp(Next, Max, nonstart, PayloadType, Seq+1, Time, SSRC, [NRI, Type])];
nal2rtp(Nal, Max, start, PayloadType, Seq, Time, SSRC, []) ->
    <<0:1/unsigned, NRI:2/unsigned, Type:5/unsigned, Remain/binary>> = Nal,
    FUI = <<0:1/unsigned, NRI:2/unsigned, 28:5/unsigned>>,
    FUH = <<1:1/unsigned,0:1/unsigned,0:1/unsigned, Type:5/unsigned>>,
    Len = Max - 2,
    <<Piece:Len/binary, Next/binary>> = Remain,
    [#rtp{payload_type = PayloadType,
         marker = 0,
         sequence_number = Seq,
         timestamp = Time,
         ssrc = SSRC,
         payload = <<FUI/binary, FUH/binary,Piece/binary>>} | nal2rtp(Next, Max, nonstart, PayloadType, Seq+1, Time, SSRC, [NRI, Type])].

%% 6fps -> 3f/500ms -> 160,160,180 -> {7,7,8}, 15000
%% 10fps -> 1f/100ms -> 100 -> {4}, 9000
%% 15fps -> 3f/200ms -> 60,60,80 -> {2,2,3}, 6000
%% 30fps -> 3f/100ms -> 20,40,40 -> {0,1,1}, 3000
video_circle(6) -> {{7,7,8}, 15000};
video_circle(6.0) -> {{7,7,8}, 15000};
video_circle(10) -> {{4}, 9000};
video_circle(10.0) -> {{4}, 9000};
video_circle(15) -> {{2,2,3}, 6000};
video_circle(15.0) -> {{2,2,3}, 6000};
video_circle(30) -> {{0,1,1}, 3000};
video_circle(30.0) -> {{0,1,1}, 3000};
video_circle(F) -> {error, F}.

frame_convert({Data, FormatFrom}, {FormatTo, State}) when FormatFrom =:= FormatTo ->
    {Data, State};
frame_convert({Data, ?AUDIO_PCM8K16}, {FormatTo, State}) when FormatTo =:= ?AUDIO_ALAW
                                                            ; FormatTo =:= ?AUDIO_ULAW ->
    NData= audio_core:convert({Data, ?AUDIO_PCM8K16}, {FormatTo, []}),
    {NData, State};
frame_convert({Data, ?AUDIO_PCM8K16}, {?AUDIO_EVS, State}) ->
    NData= audio_core:convert({Data, ?AUDIO_PCM8K16}, {?AUDIO_EVS, ?EVS_MODE}),
    {NData, State};
frame_convert({Data, FormatFrom}, {?AUDIO_PCM8K16, State}) when FormatFrom =:= ?AUDIO_ALAW
                                                              ; FormatFrom =:= ?AUDIO_ULAW ->
    NData= audio_core:convert({Data, FormatFrom}, {?AUDIO_PCM8K16, []}),
    {NData, State};
frame_convert({Data, ?AUDIO_ULAW}, {?AUDIO_ALAW, State}) ->
    NData= audio_core:convert({Data, ?AUDIO_ULAW}, {?AUDIO_ALAW, []}),
    {NData, State};
frame_convert({Data, ?AUDIO_ULAW}, {?AUDIO_EVS, State}) ->
    NData= audio_core:convert({Data, ?AUDIO_ULAW}, {?AUDIO_EVS, ?EVS_MODE}),
    {NData, State};
frame_convert({Data, ?AUDIO_ALAW}, {?AUDIO_ULAW, State}) ->
    NData= audio_core:convert({Data, ?AUDIO_ALAW}, {?AUDIO_ULAW, []}),
    {NData, State};
frame_convert({Data, ?AUDIO_ALAW}, {?AUDIO_EVS, State}) ->
    NData= audio_core:convert({Data, ?AUDIO_ALAW}, {?AUDIO_EVS, ?EVS_MODE}),
    {NData, State};
frame_convert({Data, ?AUDIO_AMRNB}, {?AUDIO_PCM8K16, State}) ->
    {NState, NData} = amr_codec:decode_frame(?AUDIO_AMRNB, State, Data),
    {NData, NState};
frame_convert({Data, ?AUDIO_AMRNB}, {?AUDIO_EVS, State}) ->
    {NState, PCM8K16} = amr_codec:decode_frame(?AUDIO_AMRNB, State, Data),
    NData = audio_core:convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_EVS, ?EVS_MODE}),
    {NData, NState};
frame_convert({Data, ?AUDIO_AMRNB}, {FormatTo, State}) when FormatTo =:= ?AUDIO_ALAW
                                                          ; FormatTo =:= ?AUDIO_ULAW ->
    {NState, PCM8K16} = amr_codec:decode_frame(?AUDIO_AMRNB, State, Data),
    NData= audio_core:convert({PCM8K16, ?AUDIO_PCM8K16}, {FormatTo, []}),
    {NData, NState};
frame_convert({Data, ?AUDIO_AMRWB}, {?AUDIO_PCM8K16, State}) ->
    {NState, PCM16K16} = amr_codec:decode_frame(?AUDIO_AMRWB, State, Data),
    NData = audio_core:convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_PCM8K16, []}),
    {NData, NState};
frame_convert({Data, ?AUDIO_AMRWB}, {?AUDIO_EVS, State}) ->
    {PCM8K16, NState} = frame_convert({Data, ?AUDIO_AMRWB}, {?AUDIO_PCM8K16, State}),
    NData = audio_core:convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_EVS, ?EVS_MODE}),
    {NData, NState};
frame_convert({Data, ?AUDIO_AMRWB}, {FormatTo, State}) when FormatTo =:= ?AUDIO_ALAW
                                                          ; FormatTo =:= ?AUDIO_ULAW ->

    {PCM8K16, NState} = frame_convert({Data, ?AUDIO_AMRWB}, {?AUDIO_PCM8K16, State}),
    NData = audio_core:convert({PCM8K16, ?AUDIO_PCM8K16}, {FormatTo, []}),
    {NData, NState};
frame_convert({Data, ?AUDIO_EVS}, {?AUDIO_PCM8K16, State}) ->
    EvsDecoder = State#state.evs_decoder,
    #audio_desc{ evs_ext = {primary, EvsMode}} = State#state.audio_desc,
    NData = evs_port:decode_frame(EvsDecoder,<<EvsMode:8, Data/binary>>),
    {NData, State};
frame_convert({Data, ?AUDIO_EVS}, {FormatTo, State}) when FormatTo =:= ?AUDIO_ALAW
                                                        ; FormatTo =:= ?AUDIO_ULAW ->
    {PCM8K16, NState} = frame_convert({Data, ?AUDIO_EVS}, {?AUDIO_PCM8K16, State}),
    NData = audio_core:convert({PCM8K16, ?AUDIO_PCM8K16}, {FormatTo, []}),
    {NData, NState}.
data_send(_Socket, _Address, _Port, <<>>) ->
    ok;
data_send(Socket, Address, Port, Packet) ->
    catch gen_udp:send(Socket, Address, Port, Packet),
    update_rtp(rtp_send, byte_size(Packet)).


clear_rtp() ->
    clear_rtp(rtp_send),
    clear_rtp(rtp_recv).

clear_rtp(Type) ->
    put({Type, size}, 0),
    put({Type, count}, 0).

update_rtp(Type, Size) ->
    NSize = get({Type, size}) + Size,
    put({Type, size}, NSize),
    NCount = get({Type, count}) + 1,
    put({Type, count}, NCount).

report_rtp(Address) ->
    report_rtp(Address, rtp_send),
    report_rtp(Address, rtp_recv).
report_rtp(Address, Type) ->
    Size = get({Type, size}),
    Count = get({Type, count}),
    Direct = case Type of
                 rtp_send -> send;
                 rtp_recv -> recv
             end,
    info_manager:rtp(Address, Direct, {Count, Size}).
