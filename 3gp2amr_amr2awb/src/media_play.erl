%%%-------------------------------------------------------------------
%%% File    : media_play.erl
%%% Author  :  <wanglihe@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created : 26 Apr 2013 by  <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(media_play).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").
-include("rtp.hrl").
-include("wave.hrl").
-include("audio_core.hrl").
-include("video_core.hrl").
-include("iso_base_file.hrl").
-include("media_desc.hrl").


-define(AMR_AMRNB, "#!AMR\n").
-define(AMR_AMRWB, "#!AMR-WB\n").
-define(EVS, "#!EVS_MC1.0\n").
-define(FILE_MAX, 16*1024*1024).

-export([cast/2]).

-define(RING_PATH, "../voice/local/").

-ifdef(max_atime).
-define(ATIMEMODIFY, ?max_atime).
-else.
-define(ATIMEMODIFY, 7).
-endif.

-record(state, { worker
               , trans
               , play_maxtime_timer = none
               , play_totaltime = 0
               , play_start_time}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link(Trans, Control, Dest, StartTimer) -> term() when
      Trans :: term(),
      Control :: term(),
      Dest :: client | pid(),
      StartTimer :: term().
start_link(Trans, Control, Dest, StartTimer) ->
    gen_server:start_link(?MODULE, [self(), Trans, Control, Dest, StartTimer], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Worker, Trans, Control, Dest, StartTimer]) ->
    process_flag(trap_exit, true),
    {AudioTrans, VideoTrans} = Trans,
    #media_ctrl{ type = Type
               , filenames = Filenames
               , times = PlayTimes
               , interval = Interval
               , max_time = TotalTime} = Control,
    %%为尽量同步播放时间，先进行数据载入
    {AudioData, VideoData} = case Type of
        audio ->
            {_,_,_,_,AudioDesc} = AudioTrans,
            {get_file_data(audio, Filenames, AudioDesc), <<>>};
        video ->
            {_,_,_,_,AudioDesc} = AudioTrans,
            {_,_,_,_,VideoDesc} = VideoTrans,
            { get_file_data(audio, Filenames, AudioDesc)
            , get_file_data(video, Filenames, VideoDesc)}
    end,

    case Type of
        audio ->
            {AudioDT, _, _, _, _} = AudioTrans,
            data_transfer:start_service(AudioDT, {send, Dest, ?MODULE, {AudioData, Interval, PlayTimes}}),
            info_manager:info("~p ~p goto playing", [Filenames, audio]);
        video when VideoData =:= <<>> ->
            {AudioDT, _, _, _, _} = AudioTrans,
            data_transfer:start_service(AudioDT, {send, Dest, ?MODULE, {AudioData, Interval, PlayTimes}}),
            info_manager:info("~p ~p goto playing", [Filenames, audio]);
        video ->
            {AudioDT, _, _, _, _} = AudioTrans,
            {VideoDT, _, _, _, _} = VideoTrans,
            data_transfer:start_service(AudioDT, {send, Dest, ?MODULE, {AudioData, Interval, PlayTimes}}),
            data_transfer:start_service(VideoDT, {send, Dest, ?MODULE, {VideoData, Interval, PlayTimes}}),
            info_manager:info("~p ~p goto playing", [Filenames, video])
    end,


    MaxTimer = gen_timer(TotalTime, play_maxtime),
    info_manager:time(time_play_delay, StartTimer),
    {ok, #state{ trans = Trans
               , play_maxtime_timer = MaxTimer
               , worker = Worker
               , play_start_time = os:timestamp()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    info_manager:info("media_play receive stop"),
    {stop, normal, State};

handle_cast(send_over, State) ->
    info_manager:info("media_play receive play over"),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({timeout, TimerRef, play_maxtime}
            , #state{play_maxtime_timer = TimerRef} = State) ->
    info_manager:info("play reach maxtime stop~n"),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{ worker = Worker
                         , play_start_time = StartTimer} = _State) ->
    PlayingTime = timer:now_diff(os:timestamp(), StartTimer),
    worker:cast(Worker, {media_play_stop, {ok, over, PlayingTime}}),
    info_manager:time(time_playing, StartTimer),
    info_manager:info("~p ~p terminate",[?MODULE, self()]).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

gen_timer(infinity, _) ->
    infinity;
gen_timer(Timeout, Msg) ->
    erlang:start_timer(Timeout, self(), Msg).

load_audio(Filenames) ->
    info_manager:info("load audio"),
    lists:map(fun load_audio_file/1, Filenames).

load_audio_file(Filename) ->
    %%ring_manager只负责将文件从铃音服务器同步，由于是第二核心，所以有可能成为
    %%性能瓶颈。但是由于我们无法设计整体的实时存储方案，此方案是目前可选的最优法
    %%未来可以考虑升级的部分是tmpfs内存磁盘，erlang内部内存缓存
    catch ring_manager:update_file(Filename),
    case file:read_file_info(?RING_PATH++Filename, [{time, universal}]) of
        {error, ReasonRead} ->
            error({error, ReasonRead});
        {ok, FileInfo} ->
            case FileInfo#file_info.size of
                Size when Size > ?FILE_MAX ->
                    error({error, max_limit});
                _ ->
                    T1 = FileInfo#file_info.atime,
                    T2 = erlang:universaltime(),
                    case calendar:time_difference(T1, T2) of
                        {Days, _} when Days >= ?ATIMEMODIFY ->
                            info_manager:info("update atime cause ~p", [Days]),
                            file:write_file_info(?RING_PATH++Filename, FileInfo#file_info{atime = T2}, [{time, universal}]);
                        _ ->
                            ok
                    end
            end
    end,
    {ok, Binary} = file:read_file(?RING_PATH++Filename),
    Suffix = lists:reverse(lists:takewhile(fun(E) ->
                                    E =/= $. end
                                , lists:reverse(Filename))),
    case Suffix of
        "awb" ->
            case Binary of
                <<?AMR_AMRWB,Data/binary>> ->
                    {Data, ?AUDIO_AMRWB};
                _ ->
                    error({eformat, bad_awb})
            end;
        "amr" ->
            case Binary of
                <<?AMR_AMRNB,Data/binary>> ->
                    {Data, ?AUDIO_AMRNB};
                <<?AMR_AMRWB,Data/binary>> ->
                    {Data, ?AUDIO_AMRWB};
                _ ->
                    error({eformat, bad_amr})
            end;
        "evs" ->
            case Binary of
                <<?EVS,1:32/big, Data/binary>> ->
                    {Data, ?AUDIO_EVS};
                <<?EVS,_:32/big, _Data/binary>> ->
                    error({eformat, bad_evs_channel});
                _ ->
                    error({eformat, bad_evs})
            end;
        "3gp" ->
            Iso = case iso_base_file:parse(Binary) of
                {error, Reason} ->
                    error({eformat, Reason});
                {IsoFile, []} ->
                    IsoFile;
                {IsoFile, Reason} ->
                    info_manager:log_debug("Audio unknown box", io_lib:format("~p: ~p", [Filename, Reason])),
                    IsoFile
            end,
            case [T ||T <-Iso#isofile.trak, T#trak.media_type =:= sound] of
                [Trak|_] ->
                    Type = case Trak#trak.sound_type of
                        amrwb -> ?AUDIO_AMRWB;
                        amrnb -> ?AUDIO_AMRNB;
                        _ ->
                            error({eformat, sound_type})
                    end,
                    #trak{ chunk_offset = Offsets
                         , chunk_sample_count = ChunkSampleCount
                         , sample_size = Samplesize} = Trak,
                    Data = list_to_binary(get_trak_data(Binary, Offsets, ChunkSampleCount, Samplesize)),
                    {Data, Type};
                [] ->
                    error({eformat, bad_amr})
            end;
        "wav" ->
            WaveInfo = case catch wave:parse(Binary) of
                {'EXIT', _} ->
                    error({eformat, bad_wav});
                {Info, []} ->
                    Info;
                {Info, Error} ->
                    info_manager:log_debug("Nonstandard wave"
                                         , io_lib:format("~p: ~p", [Filename, Error])),
                    Info
            end,
            Type = case WaveInfo#wave.wformat_code of
                ?WAVE_MULAW ->
                    ?AUDIO_ULAW;
                ?WAVE_ALAW ->
                    ?AUDIO_ALAW;
                ?WAVE_PCM ->
                    case {WaveInfo#wave.sample_rate, WaveInfo#wave.bits_per_sample} of
                        ?AUDIO_8K8BIT ->
                            ?AUDIO_PCM8K8;
                        ?AUDIO_8K16BIT ->
                            ?AUDIO_PCM8K16;
                        ?AUDIO_16K8BIT ->
                            ?AUDIO_PCM16K8;
                        ?AUDIO_16K16BIT ->
                            ?AUDIO_PCM16K16;
                        {R,B} ->
                            error({eformat, {rate, R, bits, B}})
                    end;
                T ->
                    error({eformat, {wformat_code, T}})
            end,
            {WaveInfo#wave.data , Type};
        "vox" ->
            {Binary, ?AUDIO_VOX8K};
        "ulaw" ->
            {Binary, ?AUDIO_ULAW};
        "alaw" ->
            {Binary, ?AUDIO_ALAW};
        "g729" ->
            {Binary, ?AUDIO_G729};
        Ext ->
            error({eformat, Ext})
    end.

load_video(Filenames) ->
    lists:map(fun load_video_file/1, Filenames).

load_video_file(Filename) ->
    info_manager:info("load video"),
    case file:read_file_info(?RING_PATH++Filename) of
        {error, ReasonRead} ->
            error({error, ReasonRead});
        {ok, FileInfo} ->
            case FileInfo#file_info.size of
                Size when Size > ?FILE_MAX ->
                    error({error, max_limit});
                _ ->
                    ok
            end
    end,
    {ok, Binary} = file:read_file(?RING_PATH++Filename),
    Suffix = lists:reverse(lists:takewhile(fun(E) ->
                                    E =/= $. end
                                , lists:reverse(Filename))),
    case Suffix of
        "3gp" ->
           %%audio和video都支持unknowbox,出现unknownbox写debug日志
            Iso = case iso_base_file:parse(Binary) of
                {error, Reason} ->
                    error({eformat, Reason});
                {IsoFile, []} ->
                    IsoFile;
                {IsoFile, Reason} ->
                    info_manager:log_debug("Video unknown box", io_lib:format("~p: ~p", [Filename, Reason])),
                    IsoFile
            end,
            [Trak|_] = [T ||T <-Iso#isofile.trak, T#trak.media_type =:= video],
            Type = Trak#trak.video_type,
            #trak{ chunk_offset = Offsets
                 , chunk_sample_count = ChunkSampleCount
                 , sample_size = Samplesize
                 , pps = PPS
                 , sps = SPS} = Trak,
            Nals = lists:flatmap(fun chunk2nal/1,
                                get_trak_data(Binary, Offsets, ChunkSampleCount, Samplesize)),
            %% insert sps and pps into Nals
            Nnals = insert_sps_pps(Nals, SPS, PPS),
            {Nnals, Type};

        Ext ->
            error({eformat, Ext})
    end.
get_trak_data(_Binary, _Offsets, [], _Samplesize) ->
    error({eformat, bad_trak});
get_trak_data(Binary, Offset, ChunkSamples, SampleSize) when is_integer(Offset)
                                                           , is_integer(SampleSize) ->
    get_trak_data(Binary, [Offset], ChunkSamples, SampleSize);
get_trak_data(Binary, Offsets, ChunkSamples, SampleSize) when is_integer(SampleSize) ->
    {Result, _, _, _} =
    lists:foldl(fun(Offset, {Data, Index, {_, SampleCount, _} = JustChunk, []}) ->
                        {[get_chunk_samples(Binary, Offset, SampleCount, SampleSize)|Data], Index+1, JustChunk, []};
                   (Offset, {Data, Index, {_, JustSampleCount, _} = JustChunk, NextChunkSamples}) ->
                        case hd(NextChunkSamples) of
                            {NextIndex, NextSampleCount, _} when NextIndex =< Index ->
                                {[get_chunk_samples(Binary, Offset, NextSampleCount, SampleSize)|Data], Index+1, hd(NextChunkSamples), tl(NextChunkSamples)};
                            _ ->
                                {[get_chunk_samples(Binary, Offset, JustSampleCount, SampleSize)|Data], Index+1, JustChunk, NextChunkSamples}
                        end
                end, {[], 1, hd(ChunkSamples), tl(ChunkSamples)}, Offsets),
    lists:flatten(lists:reverse(Result));

get_trak_data(Binary, Offsets, ChunkSamples, SampleSizes) when is_list(SampleSizes) ->
    {Result, _, _, _, _} =
    lists:foldl(fun(Offset, {Data, Index, {_, SampleCount, _} = JustChunk, [], Sizes}) ->
                        {[get_chunk_samples(Binary, Offset, SampleCount, Sizes)|Data], Index+1, JustChunk, [], lists:nthtail(SampleCount, Sizes)};
                   (Offset, {Data, Index, {_, JustSampleCount, _} = JustChunk, NextChunkSamples, Sizes}) ->
                        case hd(NextChunkSamples) of
                            {NextIndex, NextSampleCount, _} when NextIndex =< Index ->
                                {[get_chunk_samples(Binary, Offset, NextSampleCount, Sizes)|Data], Index+1, hd(NextChunkSamples), tl(NextChunkSamples), lists:nthtail(NextSampleCount, Sizes)};
                            _ ->
                                {[get_chunk_samples(Binary, Offset, JustSampleCount, Sizes)|Data], Index+1, JustChunk, NextChunkSamples, lists:nthtail(JustSampleCount, Sizes)}
                        end
                end, {[], 1, hd(ChunkSamples), tl(ChunkSamples), SampleSizes}, Offsets),
    lists:flatten(lists:reverse(Result)).

get_chunk_samples(_, _, 0, SampleSize) when is_integer(SampleSize) ->
    [];
get_chunk_samples(Binary, Offset, SampleCount, SampleSize) when is_integer(SampleSize) ->
    [binary_part(Binary, Offset, SampleCount*SampleSize)];
get_chunk_samples(_, _, 0, _) ->
    [];
get_chunk_samples(_, _, _, []) ->
    error({eformat, bad_trak});
get_chunk_samples(Binary, Offset, SampleCount, [SampleSize|SampleSizes]) ->
    [binary_part(Binary, Offset, SampleSize)|get_chunk_samples(Binary, Offset+SampleSize, SampleCount - 1, SampleSizes)];
get_chunk_samples(_, _, 0, _) ->
    error({eformat, bad_trak}).

%%iso-14496-15 5.2.3 定义了avc的存储结构
chunk2nal(Bin) ->
    <<Len:32/unsigned, Content/binary>> = Bin,
    case Len of
        _ when Len < byte_size(Content) ->
            <<Nal:Len/binary, Next/binary>> = Content,
            [Nal|chunk2nal(Next)];
        _ when Len =:= byte_size(Content) ->
            [Content];
        _ ->
            info_manager:info("wrong length: ~p with bin ~p~n", [Len, Bin]),
            []
    end.
get_file_data(_, _Filenames, none) ->
    <<>>;
get_file_data(audio, Filenames, AudioDesc) ->
    #audio_desc{ codec = MediaType} = AudioDesc,
    try load_audio(Filenames) of
        Files when is_list(Files) ->
            TransMode = case MediaType of
                            ?AUDIO_ALAW -> [];
                            ?AUDIO_ULAW -> [];
                            ?AUDIO_G729 -> [];
                            ?AUDIO_PCM8K16 -> [];
                            ?AUDIO_EVS ->
                                #audio_desc{ evs_ext = {primary, EvsMode}} = AudioDesc,
                                EvsMode;
                            ?AUDIO_AMRNB ->
                                #audio_desc{ amr_ext = {AMRMode, _}} = AudioDesc,
                                AMRMode;
                            ?AUDIO_AMRWB ->
                                #audio_desc{ amr_ext = {AMRMode, _}} = AudioDesc,
                                AMRMode
                        end,
            %%amr直接拼接有可能由于状态问题产生拼接爆音等音频问题．但是当前针对
            %%可变音播放以及多个info顺序播放均未发生此问题，所以暂时以最简单的方式
            %%实现拼接．未来若出现问题，可以尝试增加静音包等特殊包来处理．
            lists:foldl(fun(SrcBin, D) ->
                Converted = try audio_core:convert(SrcBin, {MediaType, TransMode})
                            catch
                                error:empty_binary ->
                                    <<>>;
                                Class:Reason ->
                                    error_logger:log_info("Media play error", "process file ~p error ~p:~p~n",[Files, Class, Reason]),
                                    exit(Reason)
                            end,
                <<D/binary, Converted/binary>>
              end, <<>>, Files);
        _ ->
            error_logger:info_msg("read file error: not match media type"),
            exit({shutdown, eformat})
    catch
        error:{badmatch, {error, enoent}} ->
            info_manager:log_error("File operation fail", io_lib:format("~p not exist", [Filenames])),
            exit({shutdown, enoent});
        error:{error, enoent} ->
            info_manager:log_error("File operation fail", io_lib:format("~p not exist", [Filenames])),
            exit({shutdown, enoent});
        error:function_clause ->
            info_manager:log_error("File operation fail", "file read error"),
            exit({shutdown, eformat});
        error:{eformat, Reason} ->
            info_manager:log_error("File operation fail", io_lib:format("~p not support",[Reason])),
            exit({shutdown, eformat});
        Class:Reason ->
            error_logger:info_msg("read file error ~p:~p~n",[Class, Reason]),
            exit({shutdown, Reason})
    end;
get_file_data(video, Filenames, VideoDesc) ->
    #video_desc{ codec = MediaType} = VideoDesc,
    try load_video(Filenames) of
        [{Nals, _}] when MediaType =:= ?VIDEO_H264 ->
            Nals;
        _ ->
            error_logger:info_msg("read file error: not match media type"),
            exit(eformat)
    catch
        error:{badmatch, {error, enoent}} ->
            info_manager:log_error("File operation fail", io_lib:format("~p not exist", [Filenames])),
            exit({shutdown, enoent});
        error:{error, enoent} ->
            info_manager:log_error("File operation fail", io_lib:format("~p not exist", [Filenames])),
            exit({shutdown, enoent});
        error:function_clause ->
            info_manager:log_error("File operation fail", "file read error"),
            exit({shutdown, eformat});
        error:{eformat, Reason} ->
            info_manager:log_error("File operation fail", io_lib:format("~p not support",[Reason])),
            exit({shutdown, eformat});
        Class:Reason ->
            error_logger:info_msg("read file error ~p:~p~n",[Class, Reason]),
            exit({shutdown, Reason})
    end.

insert_sps_pps(Nals, Sps, Pps)->
    lists:flatten(
    lists:map(fun(<<_:3/unsigned, ?NAL_UNIT_TYPE_IDR:5/unsigned, _/binary>> = IFrame) -> [Sps, Pps, IFrame];
                 (Frame) -> Frame end, Nals)).
