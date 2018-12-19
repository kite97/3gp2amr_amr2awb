%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 30 Jun 2014 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(test_conf_member).

-behaviour(gen_server).

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("audio_core.hrl").
%%-include("ips_codec.hrl").

-define(SERVER, ?MODULE).

-record(state, { refnum
               , ip
               , fake_local_ip
               , port
               , ssrc
               , socket
               , ivr_slot
               , conf_room_id
               , conf_ref_num
               , data_transfer
               , step = init}).

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
start_link(RefNum, IP, Port, SSRC, FIP, JoinParam) ->
    gen_server:start_link(?MODULE, [RefNum, IP, Port, SSRC, FIP, JoinParam], []).

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
init([RefNum, IP, Port, SSRC, FIP, {ConfRoomID, ConfRefNum}]) ->
    case gen_udp:open(Port, [binary,{active,false}, inet, {ip, IP}]) of
        {ok, Socket} ->
            {ok, Pid} = data_transfer:start_link(Socket, SSRC),
            ICP = icp:encode(alloc, {RefNum}),
            ocarina_tester:send_icp(ICP),
            {ok, #state{ refnum = RefNum
                       , ip = IP
                       , fake_local_ip = FIP
                       , port = Port
                       , ssrc = SSRC
                       , socket = Socket
                       , conf_room_id = ConfRoomID
                       , conf_ref_num = ConfRefNum
                       , data_transfer = Pid}};
        _ ->
            {stop, bad}
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
%%handle_cast({alloc, 0, {RefNum, IvrSlot}, AddrPort}, #state{ fake_local_ip = FIP
%%                                                           , port = Port
%%                                                           , conf_room_id = ConfRoomID
%%                                                           , conf_ref_num = ConfRefNum} = State) ->
%%    %%info_manager:info("got alloc result, send ack"),
%%    Media = {?MEDIA_ULAW, 0},
%%    %%Media = {?MEDIA_AMRNB, 102},
%%    DC = {2, 101},
%%    Seq = 0,
%%    Ack = icp:encode(ack, {{RefNum, IvrSlot}, {FIP, Port}, Media, DC, Seq}),
%%    ocarina_tester:send_icp(Ack),
%%
%%    MediaAttribute = ?MEDIA_ULAW,
%%    PayloadType = 0,
%%    %%MediaAttribute = ?MEDIA_AMRNB,
%%    %%PayloadType = 102,
%%
%%    DCModeCode = 2,
%%    PT_2833 = 101,
%%    Rtp = {rtp, [ {client, AddrPort}
%%                , {mediatype, MediaAttribute}
%%                , {payloadtype, PayloadType}
%%                , {dc_mode, {DCModeCode, PT_2833}}]},
%%    data_transfer:set(State#state.data_transfer, Rtp),
%%
%%    info_manager:info("add ~p to conf ~p", [RefNum, ConfRefNum]),
%%    Join = icp:encode({control, {join, {ConfRoomID, {1, IvrSlot}}, 0}}, ConfRefNum),
%%    ocarina_tester:send_icp(Join),
%%
%%    {noreply, State#state{step = after_ack
%%                        , ivr_slot = IvrSlot}};

handle_cast({service, _, _RefNum}, State) when State#state.step =:= after_ack ->
    info_manager:info("got ack result, begin play"),

    {_A, _B, C} = os:timestamp(),
    timer:sleep((C rem 5)*1000),

    Filenames = ["perftest.wav"],
    MediaType = ?AUDIO_ULAW,
    %%Filenames = ["perftest.amr"],
    %%MediaType = ?AUDIO_AMRNB,
    AMRMode = 7,
    SSRC = State#state.ssrc,
    Media = {Filenames, MediaType, AMRMode, SSRC},
    PlayTimes = 100,
    Interval = 0,
    TotalTime = 60*1000,
    PlayPara = {PlayTimes, Interval, TotalTime},
    %%MsgPara = {RefNum, MetaData, COMMAND_H},
    MsgPara = {1,2,3},
    SrcPid = self(),
    DataTransfer = State#state.data_transfer,
    StartTimer = false,
    {ok, _Pid} = media_play:start_link(DataTransfer, Media, PlayPara, StartTimer, MsgPara, SrcPid),

    {noreply, State#state{step = self_play}};
handle_cast({play, over}, State) ->
    info_manager:info("play, over"),
    RefNum = State#state.refnum,
    Release = icp:encode(release, {RefNum, State#state.ivr_slot}),
    ocarina_tester:send_icp(Release),
    {noreply, State};
handle_cast({release, _, _RefNum}, State) ->
    %%info_manager:info("~p got release", [RefNum]),
    {stop, normal, State};

handle_cast({data, _, _}, State) ->
    {noreply, State};
handle_cast(Msg, State) ->
    info_manager:info("~p, got ~p", [?MODULE, Msg]),
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
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
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
