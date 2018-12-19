-module(mixer_agent).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../include/rtp.hrl").
-include("../include/audio_core.hrl").

-export([cast/2]).

-define(SERVER, ?MODULE).

-record(state, { mixer
               , data_transfer
               , conf_member_id
               , amr_estate = none
               , amr_dstate = none
               , media_type = none}).


%%%------------------------------     API     -----------------------------------%%%
start_link(IMPConfMembID, DataTransfer) ->
    gen_server:start_link(?MODULE, [self(), IMPConfMembID, DataTransfer], []).


%%%------------------------------    init    -----------------------------------%%%
init([Mixer, IMPConfMembID, DataTransfer]) ->
    process_flag(trap_exit, true),
    info_manager:info("~p ~p init~n", [?MODULE, self()]),
    {ok, MediaType} = data_transfer:start_service(DataTransfer, {listen, data}),
    data_transfer:start_service(DataTransfer, {listen, digit}),
    case MediaType of
        _ when MediaType =:= ?AUDIO_AMRNB; MediaType =:= ?AUDIO_AMRWB ->
            {ok, #state{ mixer = Mixer
                       , data_transfer = DataTransfer
                       , conf_member_id = IMPConfMembID
                       , amr_estate = amr_codec:init(MediaType, encode)
                       , amr_dstate = amr_codec:init(MediaType, decode)
                       , media_type = MediaType}};
        _ when MediaType =:= ?AUDIO_ULAW; MediaType =:= ?AUDIO_ALAW ->
            {ok, #state{ mixer = Mixer
                       , data_transfer = DataTransfer
                       , conf_member_id = IMPConfMembID
                       , media_type = MediaType}};
        _ ->
            {stop, bad_media_type}
    end.


%%%------------------------------ handle_call -----------------------------------%%%
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%%------------------------------ handle_cast -----------------------------------%%%
handle_cast({data, {_PayloadType, _Timestamp}, Data}, State) ->
    {NDState, PCMData} = convert_data({to_mixer, State#state.media_type, State#state.amr_dstate}, Data),
    case byte_size(PCMData) of
        Size when Size =:= 320 ->
            mixer:cast(State#state.mixer, {data, State#state.conf_member_id, self(), PCMData});
        Size when Size > 320 ->
            <<NPCMData:320/binary, _Rest/binary>> = PCMData,
            mixer:cast(State#state.mixer, {data, State#state.conf_member_id, self(), NPCMData});
        Size when Size < 320 ->
            RestSize = 8 * (320 - Size),
            NPCMData = <<PCMData/binary, 0:RestSize>>,
            mixer:cast(State#state.mixer, {data, State#state.conf_member_id, self(), NPCMData})
    end,
    {noreply, State#state{amr_dstate = NDState}};

handle_cast({digit, Digit}, State) ->
    info_manager:info("mixer_agent ~p get a digit ~p, send it to mixer~n", [self(), Digit]),
    mixer:cast(State#state.mixer, {digit, State#state.conf_member_id, self(), Digit}),
    {noreply, State};

handle_cast({mixed_data, Data}, State) when State#state.media_type =/= none ->
    {NEState, RTPData} = convert_data({from_mixer, State#state.media_type, State#state.amr_estate}, Data),
    data_transfer:cast(State#state.data_transfer, {send, {mixed_data, RTPData, self()}}),
    {noreply, State#state{amr_estate = NEState}};

handle_cast(stop, #state{data_transfer = DataTransfer} = State) ->
    catch data_transfer:stop_service(DataTransfer, {listen, data}),
    info_manager:info("~p ~p stop~n", [?MODULE, self()]),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%%------------------------------ handle_info -----------------------------------%%%
handle_info(_Info, State) ->
    {noreply, State}.


%%%------------------------------  terminate  -----------------------------------%%%
terminate(_Reason, _State) ->
    ok.


%%%------------------------------ code_change -----------------------------------%%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%------------------------------     API     -----------------------------------%%%
cast(MixerAgent, Msg) ->
    gen_server:cast(MixerAgent, Msg).



%%%------------------------------ Internal API -----------------------------------%%%
convert_data(DesType, Data) ->
    case DesType of
               {to_mixer, MediaType, DState} when MediaType =:= ?AUDIO_AMRNB; MediaType =:= ?AUDIO_AMRWB ->
                   amr_codec:decode_frame(MediaType, DState, Data);
               {to_mixer, MediaType, _} ->
                   {none, audio_core:convert({Data, MediaType}, {?AUDIO_PCM8K16, []})};
               {from_mixer, MediaType, EState} when MediaType =:= ?AUDIO_AMRNB; MediaType =:= ?AUDIO_AMRWB ->
                   amr_codec:encode_frame(MediaType, EState, Data, 7);
               {from_mixer, MediaType, _} ->
                   {none, audio_core:convert({Data, ?AUDIO_PCM8K16}, {MediaType, []})}
    end.
