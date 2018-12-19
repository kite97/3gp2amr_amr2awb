%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 30 Jun 2014 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(test_conf).

-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("audio_core.hrl").
%%-include("ips_codec.hrl").

-record(state, { refnum
               , ip
               , port
               , fake_local_ip
               , ssrc
               , socket
               , ivr_slot
               , data_transfer
               , room
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
start_link(RefNum, IP, Port, SSRC, FIP) ->
    gen_server:start_link(?MODULE, [RefNum, IP, Port, SSRC, FIP], []).

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
init([RefNum, IP, Port, SSRC, FIP]) ->
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
%%handle_cast({alloc, 0, {RefNum, IVRSlot}, _AddrPort}, #state{ fake_local_ip = FIP
%%                                                 , port = Port} = State) ->
%%    Media = {?MEDIA_AMRNB, 102},
%%    DC = {2, 101},
%%    Seq = 0,
%%    Ack = icp:encode(ack, {{RefNum, IVRSlot}, {FIP, Port}, Media, DC, Seq}),
%%    ocarina_tester:send_icp(Ack),
%%    {noreply, State#state{step = after_ack, ivr_slot = IVRSlot}};
handle_cast({service, _, RefNum}, State) when State#state.step =:= after_ack ->
%    io:format("after ack, send creatconf~n"),
    CreateConf = icp:encode(create, RefNum),
    ocarina_tester:send_icp(CreateConf),
    {noreply, State#state{step = after_create}};
handle_cast({create, 0, {RefNum, ConfRoomID}}, State) ->
    info_manager:info("get create resp"),
    %%REC = icp:encode({control, {start_rec, {ConfRoomID, {integer_to_list(RefNum)}}, 0}}, RefNum),
    %%ocarina_tester:send_icp(REC),
    {ok, _CapsTimer} = timer:send_after(1000*10, time_pulse),
    lists:map(fun(_) ->
                gen_server:call(tester_pool, {new_join, {ConfRoomID, RefNum}, test_conf_member}),
                ok end
            , lists:seq(1,3)),
    {noreply, State#state{step = after_create_succ
                          , room = ConfRoomID}};

handle_cast({destory, _, _}, State) ->
    info_manager:info("destory succ~n"),
    RefNum = State#state.refnum,
    Release = icp:encode(release, {RefNum, State#state.ivr_slot}),
    ocarina_tester:send_icp(Release),
    {noreply, State};

handle_cast({release, _, _RefNum}, State) ->
    %%info_manager:info("~p got release", [RefNum]),
    {stop, normal, State};
handle_cast(time_pulse, State) when State#state.step =:= after_create_succ ->
    info_manager:info("go to destory"),
    RefNum = State#state.refnum,
    STOPREC = icp:encode({control, {stop_rec, {State#state.room, {[]}}, 0}}, RefNum),
    ocarina_tester:send_icp(STOPREC),
    ConfRoomID = State#state.room,
    DestoryConf = icp:encode({destory, ConfRoomID}, RefNum),
    ocarina_tester:send_icp(DestoryConf),
   {noreply, State#state{step = after_destory}};

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
handle_info(time_pulse, State) when State#state.step =:= after_create_succ ->
    info_manager:info("go to destory"),
    ConfRoomID = State#state.room,
    RefNum = State#state.refnum,
    DestoryConf = icp:encode({destory, ConfRoomID}, RefNum),
    ocarina_tester:send_icp(DestoryConf),
   {noreply, State#state{step = after_destory}};
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
