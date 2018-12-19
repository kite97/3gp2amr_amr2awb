%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 24 Oct 2013 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(media_record).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([cast/2]).

-define(SERVER, ?MODULE).

-include("rtp.hrl").
-include("audio_core.hrl").
-include("video_core.hrl").
-include("media_desc.hrl").
-include("wave.hrl").
-include("iso_base_file.hrl").

-define(AMR_AMRNB, "#!AMR\n").
-define(AMR_AMRWB, "#!AMR-WB\n").
-define(EVS, "#!EVS_MC1.0\n").
-define(RECORD_PATH, "../voice/record/").

-record(state, { worker
               , msg_para
               , record_start_time
               , media
               , file
               , write_state = none
               , termkey
               , maxtime
               , barge
               , prespeech
               , postspeech
               , data_buffer = []
               , src_pid
               , result}).

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
-spec start_link(Trans, RecPara, Src, StartTimer) -> term() when
      Trans :: term(),
      RecPara:: term(),
      Src :: client | mixer,
      StartTimer :: term().
start_link(Trans, RecPara, Src, StartTimer) ->
    gen_server:start_link(?MODULE, [self(), Trans, RecPara, Src, StartTimer], []).

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
init([Worker, Trans, RecPara, Src, StartTimer]) ->
    process_flag(trap_exit, true),
    #record_ctrl{ file_name = FileName
                , format = [Format]
                , barge = Barge
                , term_key = TermKey
                , max_time = MaxTime} = RecPara,
    {DataTransfer, _, _, _, _} = Trans,
    case MaxTime of
        infinity ->
            ok;
        _ ->
            erlang:start_timer(MaxTime, self(), rec_maxtime)
    end,
    info_manager:time(time_record_delay, StartTimer),
    data_transfer:start_service(DataTransfer, {listen, {digit, Src}}),
    data_transfer:start_service(DataTransfer, {listen, {data, Src, Format}}),
    info_manager:info("rec media type: ~p",[Format]),
    {ok, #state{ worker = Worker
               , record_start_time = os:timestamp()
               , media = Format
               , file = FileName
               , write_state = wait
               , termkey = TermKey
               , maxtime = MaxTime
               , barge = Barge}}.

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
handle_cast({data, _, _Attribute, Data}, #state{data_buffer = Buffer} = State) ->
    {noreply, State#state{data_buffer = [Data|Buffer]}};
handle_cast({digit, Digit}, #state{ termkey = Digit
                                  , media = MediaType
                                  , file = File
                                  , write_state = wait
                                  , record_start_time = Timer
                                  , data_buffer = Data} = State) ->
    info_manager:info("rec get termkey~n"),
    Result = case write_to_file(File, MediaType, Data) of
        ok ->
            {ok, termkey, timer:now_diff(os:timestamp(), Timer) div 1000000};
        {error, Reason} ->
            {error, Reason}
    end,
    {stop, normal, State#state{result = Result, write_state = written}};
handle_cast({digit, _Digit}, #state{barge = true} = State) ->
    info_manager:info("rec get barge"),
    worker:cast(State#state.worker, record_media_barge),
    {noreply, State};
handle_cast(stop, #state{ file = File, write_state = wait, media = MediaType
                        , data_buffer = Data, record_start_time = Timer} = State) ->
    info_manager:info("rec receive stop~n"),
    Result = case write_to_file(File, MediaType, Data) of
        ok ->
            {ok, stopped, timer:now_diff(os:timestamp(), Timer) div 1000000};
        {error, Reason} ->
            {error, Reason}
    end,
    {stop, normal, State#state{result = Result, write_state = written}};
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
handle_info({timeout, _TimerRef, rec_maxtime}, #state{ media = MediaType, file = File, write_state = wait
                                                     , data_buffer = Data, record_start_time = Timer} = State) ->
    info_manager:info("rec receive maxtime~n"),
    Result = case write_to_file(File, MediaType, Data) of
        ok ->
            {ok, maxtime, timer:now_diff(os:timestamp(), Timer) div 1000000};
        {error, Reason} ->
            {error, Reason}
    end,
    {stop, normal, State#state{result = Result, write_state = written}};

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
terminate(Reason, #state{ media = MediaType, file = File, write_state = wait
                        , data_buffer = Data, record_start_time = Timer} = State) ->
    Result = case write_to_file(File, MediaType, Data) of
        ok ->
            {ok, release, timer:now_diff(os:timestamp(), Timer) div 1000000};
        {error, Reason} ->
            {error, Reason}
    end,
    terminate(Reason, State#state{result = Result, write_state = written});
terminate(_Reason, #state{ result = Result
                         , worker = Worker} = State) ->
    info_manager:info("~p ~p terminates, Result: ~p~n", [?MODULE, self(), Result]),
    worker:cast(Worker, {record_media_stop, Result}),
    StartTimer = State#state.record_start_time,
    info_manager:time(time_recording, StartTimer).

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
write_to_file(Filename, MediaType, Buffer) when MediaType =:= ?AUDIO_ULAW
                                              ; MediaType =:= ?AUDIO_ALAW
                                              ; MediaType =:= ?AUDIO_PCM8K16 ->
    Data = list_to_binary(lists:reverse(Buffer)),
    Wave = #wave{ wformat_code = case MediaType of
                                     ?AUDIO_ULAW -> ?WAVE_MULAW;
                                     ?AUDIO_ALAW -> ?WAVE_ALAW;
                                     ?AUDIO_PCM8K16 -> ?WAVE_PCM
                                 end
                , sample_rate = 8000
                , bits_per_sample = case MediaType of
                                        ?AUDIO_PCM8K16 -> ?BITS_16;
                                        _G711 -> ?BITS_8
                                    end
                , data = Data},
    Binary = wave:create(Wave),
    file:write_file(?RECORD_PATH++Filename, Binary);
write_to_file(Filename, ?AUDIO_AMRNB, Buffer) ->
    Data = list_to_binary(lists:reverse(Buffer)),
    NBMax = audio_core:convert({Data, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, max}),
    Binary = <<?AMR_AMRNB, NBMax/binary>>,
    file:write_file(?RECORD_PATH++Filename, Binary);
write_to_file(Filename, ?AUDIO_AMRWB, Buffer) ->
    Data = list_to_binary(lists:reverse(Buffer)),
    PCM16K16 = audio_core:convert({Data, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []}),
    WBMax = audio_core:convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, max}),
    Binary = <<?AMR_AMRWB, WBMax/binary>>,
    file:write_file(?RECORD_PATH++Filename, Binary);
write_to_file(Filename, ?AUDIO_EVS, Buffer) ->
    Data = list_to_binary(lists:reverse(Buffer)),
    Binary = <<?EVS, 1:32/big, Data/binary>>,
    file:write_file(?RECORD_PATH++Filename, Binary);
write_to_file(Filename, ?VIDEO_H264, Buffer) ->
    RtpPayloads = lists:reverse(Buffer),
    H264Nals = lists:foldl(fun rtp2nalh264/2, [<<>>], RtpPayloads),
    Data = list_to_binary(lists:reverse(H264Nals)),
    file:write_file(?RECORD_PATH++Filename, Data);
write_to_file(_Filename, MediaType, _Buffer) ->
    {error, {rec_nosupport, MediaType}}.

rtp2nalh264(Payload, [H|T]=L) ->
    case Payload of
        <<0:1/unsigned, _NRI:2/unsigned, Type:5/unsigned, _Remain/binary>>
                when Type =:= ?NAL_UNIT_TYPE_SEI;
                     Type =:= ?NAL_UNIT_TYPE_SPS;
                     Type =:= ?NAL_UNIT_TYPE_PPS;
                     (1 =< Type) and (Type =< 23) ->
            [<<1:32/unsigned-big, Payload/binary>>|L];
        %% 新NAL的片段
        <<0:1/unsigned, NRI:2/unsigned, _TYPEFUI:5/unsigned, %% FUI
          Start:1/unsigned,_E:1/unsigned,0:1/unsigned, Type:5/unsigned, %% FUH
          Nal/binary>> when Start =:= 1 ->
            [<<1:32/unsigned-big, 0:1/unsigned, NRI:2/unsigned, Type:5/unsigned, Nal/binary>> |L];
        %% 旧NAL的片段
        <<0:1/unsigned, _NRI:2/unsigned, _TYPEFUI:5/unsigned, %% FUI
          Start:1/unsigned,_E:1/unsigned,0:1/unsigned, _Type:5/unsigned, %% FUH
          Nal/binary>> when Start =:= 0 ->
            [<<H/binary,Nal/binary>> |T];
        _ ->
            %%io:format("~p ~p ~p~n", ["get", Payload, "not match"]),
            L
    end.
