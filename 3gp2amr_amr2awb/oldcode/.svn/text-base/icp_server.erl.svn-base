%%%-------------------------------------------------------------------
%%% File    : icp_server.erl
%%% Author  :  <wanglihe@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created :  7 Jan 2013 by  <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(icp_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([cast/1, parse_service_command/1, parse_control_command/1]).

-define(SERVER, ?MODULE).
-define(INFINITY, 0).

-define(heart_timeout, 3000).

-include("ips_codec.hrl").
-include("audio_core.hrl").
-include("video_core.hrl").
-include("dc_ctl.hrl").
-include("media_desc.hrl").
-include("sdp.hrl").

-record(state, { socket
               , rest = <<>>}).
-record(evs, { addr
             , port
             , type
             , payload
             , digit
             , evs_mode_switch
             , evs_bitrate}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Sock) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Sock], []).

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
init([Sock]) ->
    process_flag(trap_exit, true),
    erlang:port_connect(Sock, self()),
    case inet:setopts(Sock, [{active, true}]) of
        {error, R} ->
            exit(R);
        _ ->
            ok
    end,
    case timer:send_interval(?heart_timeout, heart) of
        {error, Reason} ->
            exit(Reason);
        {ok, _} ->
            ok
    end,
    info_manager:info("~p start~n",[?MODULE]),
    {ok, #state{socket = Sock}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, {ok}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast( {service, _IVRSlot, {play, Result}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    BinReturn = case Result of
        {ok, over} ->
            <<16#101:16/little-unsigned>>; %%正常放音结束，放音达到最大时长，停止或打断放音
        {error, enoent} ->
            <<16#14:16/little-unsigned>>;  %%文件不存在
        {error, eformat} ->
            <<16#15:16/little-unsigned>>;  %%文件格式不支持
        {error, Reason} ->
            info_manager:info("icp server get play error ~p~n", [Reason]),
            <<16#15:16/little-unsigned>>  %%未知原因错误，基本可以确定为格式错误，统一回这个
    end,
    NCommand = <<COMMAND_H/binary
               , BinReturn/binary>>,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IVRSERVICE_CNF, NCommand),
    report_icp(result_to_action(play, Result)),
    {noreply, State};

handle_cast( {play_media, _RefNum, Result, {rcs, {MetaData, COMMAND_H}, _TimerStart}}
           , #state{socket = Socket} = State) ->
    BinReturn = case Result of
        {ok, over, _} ->
            <<16#101:16/little-unsigned>>; %%正常放音结束，放音达到最大时长，停止或打断放音
        {error, enoent} ->
            <<16#14:16/little-unsigned>>;  %%文件不存在
        {error, eformat} ->
            <<16#15:16/little-unsigned>>;  %%文件格式不支持
        {error, Reason} ->
            info_manager:info("icp server get play error ~p~n", [Reason]),
            <<16#15:16/little-unsigned>>  %%未知原因错误，基本可以确定为格式错误，统一回这个
    end,
    NCommand = <<COMMAND_H/binary
               , BinReturn/binary>>,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IVRSERVICE_CNF, NCommand),
    report_icp(result_to_action(play, Result)),
    {noreply, State};

handle_cast( {service, _IVRSlot, {dc, Result}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    info_manager:info("process_dc pid: ~p~n",[self()]),
    BinReturn = case Result of
        {ok, timeout, Digits} ->
            info_manager:info("command digit collect timeout ~p~n",[Digits]),
            <<16#07:16/little-unsigned, (digits_to_binary(Digits))/binary>>;
        {ok, maxlen, Digits} ->
            info_manager:info("command digit collect maxlen  ~p~n",[Digits]),
            <<16#102:16/little-unsigned, (digits_to_binary(Digits))/binary>>;
        {ok, termkey, Digits} ->
            info_manager:info("command digit collect termkey ~p~n",[Digits]),
            <<16#102:16/little-unsigned, (digits_to_binary(Digits))/binary>>;
        {ok, stopped, _} ->
            info_manager:info("digit collect stop~n"),
            <<16#102:16/little-unsigned>>; %%目前不确定回什么值，rps端由于是stop，丢弃此消息
        {error, wrong_length, Digits} ->
            info_manager:info("command digit collect wrong_length ~p~n",[Digits]),
            <<16#1c:16/little-unsigned, (digits_to_binary(Digits))/binary>>;
        {error, earg, _Digits} ->
            <<16#03:16/little-unsigned>>;
        {error, _R, _Digits} ->
            <<16#1c:16/little-unsigned>> %%不知回啥，测试时再说
    end,
    info_manager:info("dc over~n"),
    NCommand = <<COMMAND_H/binary
               , BinReturn/binary>>,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IVRSERVICE_CNF, NCommand),
    info_manager:info("*****dc send rps reply***** ~p~n",[byte_size(NCommand)]),
    report_icp(result_to_action(dc, Result)),
    {noreply, State};

handle_cast( {service, _IVRSlot, {rec, Result}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    BinReturn = case Result of
        {ok, Rslt} ->
            <<16#103:16/little-unsigned, (rec_result_to_bin(Rslt))/binary>>;
        {error, _} ->
            <<16#20:16/little-unsigned>>   %%ips只有一种错误可用，后续细化
    end,
    info_manager:info("rec over ~p~n",[BinReturn]),
    NCommand = <<COMMAND_H/binary
               , BinReturn/binary>>,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IVRSERVICE_CNF, NCommand),
    info_manager:info("*****rec send rps reply***** ~p~n",[byte_size(NCommand)]),
    report_icp(result_to_action(rec, Result)),
    {noreply, State};

handle_cast( {alloc_media_ack, _IVRSlot, {ack, _Result}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->

            %%这前的注释，暂时还未确认新架构是不是可以解决
            %%
            %%由于在ack返回之前，rps可能就下发了relase,
            %%至使ack相关流程的set会找不到worker或是处理
            %%会发现worker已经'DOWN'，gen_server:call会
            %%给出异常{normal ...

            %%曾经ack是不会出错的假设不成立了，也就是说
            %%ack可能返回错误。暂时将其记为bug，处理上
            %%什么都不做，待完成返回错误的相关代码完善
            %%后加入错误处理
    NCommand = <<COMMAND_H/binary
               , 0:16>>, %%命令成功
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IVRSERVICE_CNF, NCommand),
    info_manager:info("*****ack send rps reply***** ~p~n",[byte_size(NCommand)]),
    report_icp(icp_ack_succ),
    {TimerStart,_,_} = MetaData,
    info_manager:time(time_confirm_expend, TimerStart),
    {noreply, State};
handle_cast( {alloc_media, RefNum, {ok, IPVer, IP, [Port]}, MetaData}
           , #state{socket = Socket} = State) ->
    {{IP1, IP2, IP3, IP4}, IPV6} = case IPVer of
        inet ->
            info_manager:info("local ip is ~p~n",[IP]),
            {IP, <<>>};
        inet6 ->
            info_manager:info("local ip is ~p~n",[IP]),
            { {0,0,0,0}
            , list_to_binary(
                  lists:flatten([
                      [ (X band 16#FFFF) bsr 8
                      , X band 16#FF] || X <-tuple_to_list(IP)]))}
    end,
    info_manager:info("worker port ~p~n",[Port]),
    info_manager:info("worker ipv4 ~p~n",[{IP1, IP2, IP3, IP4}]),
    info_manager:info("worker ipv6 ~p~n",[IPV6]),
    Content = << IP4:8/unsigned         %ip
               , IP3:8/unsigned
               , IP2:8/unsigned
               , IP1:8/unsigned

               , Port:16/unsigned-little%port

               , 96:8/unsigned          %payload
               , 20:8/unsigned          %packettime
               , IPV6/binary>>,
    Len = byte_size(Content),
    RtpPara = <<16#94:8/unsigned       %id
              , Len:8/unsigned         %length
              , Content/binary>>,

    NCommand = <<RefNum:32/little-unsigned
                ,Port:16/little-unsigned        %IVRSlot = Port
                ,0:16                  %result
                ,RtpPara/binary>>,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_MEDIARES_ALLOC_CNF, NCommand),
    info_manager:info("*****alloc send rps reply***** ~p~n",[byte_size(NCommand)]),
    report_icp(icp_alloc_succ),
    {TimerStart,_,_} = MetaData,
    info_manager:time(time_alloc_expend, TimerStart),
    {noreply, State};
handle_cast( {alloc_media, RefNum, {error, _Reason}, MetaData}
           , #state{socket = Socket} = State) ->
    NCommand = <<RefNum:32/little-unsigned
               , 0:16/little-unsigned           %% IVRSlot should be 0 when alloc operation fails
               , 16#12:16/little-unsigned>>,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_MEDIARES_ALLOC_CNF, NCommand),
    report_icp(icp_alloc_fail),
    {noreply, State};
handle_cast( {release, RefNum, IVRSlot, _Result, MetaData}
           , #state{socket = Socket} = State) ->
    NCommand = <<RefNum:32/little-unsigned
               , IVRSlot:16/little-unsigned      %solt
               , 0:16>>,               %result
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IVRRELEASE_CNF, NCommand),
    info_manager:info("*****release send rps reply***** ~p~n",[byte_size(NCommand)]),
    report_icp(icp_release_succ),
    {TimerStart,_,_} = MetaData,
    info_manager:time(time_release_expend, TimerStart),
    {noreply, State};

handle_cast( {service, _IVRSlot, {stop, Result}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    %%stop play stop dc and stop rec
    info_manager:info("stop recv result ~p", [Result]),
    NCommand = <<COMMAND_H/binary
               , 0:16>>, %%命令成功
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IVRSERVICE_CNF, NCommand),
    report_icp(result_to_action(stop, Result)),
    {noreply, State};

handle_cast({alloc_conf, ConfRoomID, Result, {RefNum, MetaData}}, #state{socket = Socket} = State) ->
    NCommand = case Result of
        {ok} ->
                report_icp(icp_create_succ),
                <<RefNum:32/little-unsigned
                , ConfRoomID:16/little-unsigned
                , 0:16>>;                %result
        {error, _Reason} ->
                report_icp(icp_create_fail),
                <<RefNum:32/little-unsigned
                , 0:16/little-unsigned
                , 16#f5:16/little-unsigned>>
    end,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCREATE_CNF, NCommand),
    info_manager:info("*****create send rps reply***** ~p~n",[byte_size(NCommand)]),
    {noreply, State};

handle_cast( {release_conf, ConfRoomID, _Result, {RefNum, MetaData}}
           , #state{socket = Socket} = State) ->
    report_icp(icp_destroy_succ),
    NCommand = <<RefNum:32/little-unsigned
               , ConfRoomID:16/little-unsigned
               , 0:16>>,   %result
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFDELETE_CNF, NCommand),
    info_manager:info("*****destory send rps reply***** ~p~n",[byte_size(NCommand)]),
    {noreply, State};

handle_cast( {control, {play, Result}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    NCommand = case Result of
                   {ok, over} ->
                       report_icp(icp_conf_pa_succ),
                       info_manager:info("IP Conf background play succeeded~n",[]),
                       <<COMMAND_H/binary, 0:16/little-unsigned, 0:16/little-unsigned>>;
                   {error, Reason} ->
                       report_icp(icp_conf_pa_fail),
                       error_logger:info_msg("IP Conf background play failed. Reason: ~p~n",[Reason]),
                       <<COMMAND_H/binary, 0:16/little-unsigned, 16#f5:16/little-unsigned>>
               end,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCONTROL_CNF, NCommand),
    {noreply, State};

handle_cast( {control, {rec, Result}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    NCommand = case Result of
                   {ok, Event} ->
                       report_icp(icp_conf_rec_succ),
                       info_manager:info("IP Conf record succeeded by ~p~n",[Event]),
                       <<COMMAND_H/binary, 0:16/little-unsigned, 0:16/little-unsigned, (rec_result_to_bin(Event))/binary>>;
                   {error, Reason} ->
                       report_icp(icp_conf_rec_fail),
                       error_logger:info_msg("IP Conf record failed. Reason: ~p~n",[Reason]),
                       <<COMMAND_H/binary, 0:16/little-unsigned, 16#f5:16/little-unsigned>>
               end,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCONTROL_CNF, NCommand),
    {noreply, State};

handle_cast( {control, {stop, {Result, Media}}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    NCommand = case Result of
                   ok ->
                       info_manager:info("IP Conf ~p stopped successfully~n",[Media]),
                       <<COMMAND_H/binary, 0:16/little-unsigned, 0:16/little-unsigned>>;
                   {error, Reason} ->
                       error_logger:info_msg("IP Conf Stop media failed. Reason: ~p~n",[Reason]),
                       <<COMMAND_H/binary, 0:16/little-unsigned, 16#f5:16/little-unsigned>>
               end,
    report_icp(result_to_action(conf_stop, {Result, Media})),
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCONTROL_CNF, NCommand),
    {noreply, State};

handle_cast( {control, {modify, {ConfMembID, Result}}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    NCommand = case Result of
                   ok ->
                       info_manager:info("IP Conf modify member attribute successful~n",[]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 0:16/little-unsigned>>;
                   {error, Reason} ->
                       error_logger:info_msg("IP Conf modify member attribute failed. Reason: ~p~n",[Reason]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 16#f5:16/little-unsigned>>
               end,
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCONTROL_CNF, NCommand),
    {noreply, State};

handle_cast( {event, {digit, Digit, ConfMembID}, {MetaData, RefNum, ConfRoomID}}
           , #state{socket = Socket} = State) ->
    NCommand = <<RefNum:32/little-unsigned, ConfRoomID:16/little-unsigned, 2:16/little-unsigned, 0:16/little-unsigned, ConfMembID:16/little-unsigned, (digits_to_binary([Digit]))/binary>>,
    info_manager:info("send conf Pc result to RPS: ~p~n",[NCommand]),
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFEVENT_IND, NCommand),
    {noreply, State};

handle_cast( {control, {{Action, Result}, {ConfMembID, IVRSlot}}, {MetaData, COMMAND_H}}
           , #state{socket = Socket} = State) ->
    NCommand = case Result of
                   ok ->
                       info_manager:info("IP Conf ~p succeeded~n",[Action]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 0:16/little-unsigned, 16#7f:8, 2:16/little-unsigned, IVRSlot:16/little-unsigned>>;
                   {error, Reason} ->
                       error_logger:info_msg("IP Conf ~p failed. Reason: ~p~n",[Action, Reason]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 16#f5:16/little-unsigned, 16#7f:8, 2:16/little-unsigned, IVRSlot:16/little-unsigned>>
               end,
    report_icp(result_to_action(Action, Result)),
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCONTROL_CNF, NCommand),
    {noreply, State};
handle_cast( {join_conf, ConfMembID, Result, {COMMAND_H,MetaData}}
           , #state{socket = Socket} = State) ->
    NCommand = case Result of
                   {ok} ->
                       info_manager:info("IP Conf ~p succeeded~n",[join_conf]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 0:16/little-unsigned, 16#7f:8, 2:16/little-unsigned, ConfMembID:16/little-unsigned>>;
                   {error, Reason} ->
                       error_logger:info_msg("IP Conf ~p failed. Reason: ~p~n",[join_conf, Reason]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 16#f5:16/little-unsigned, 16#7f:8, 2:16/little-unsigned, ConfMembID:16/little-unsigned>>
               end,
    %%report_icp(result_to_action(Action, Result)),
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCONTROL_CNF, NCommand),
    {noreply, State};
handle_cast( {unjoin_conf, ConfMembID, Result, {COMMAND_H,MetaData}}
           , #state{socket = Socket} = State) ->
    NCommand = case Result of
                   {ok} ->
                       info_manager:info("IP Conf ~p succeeded~n",[unjoin_conf]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 0:16/little-unsigned, 16#7f:8, 2:16/little-unsigned, ConfMembID:16/little-unsigned>>;
                   {error, Reason} ->
                       error_logger:info_msg("IP Conf ~p failed. Reason: ~p~n",[unjoin_conf, Reason]),
                       <<COMMAND_H/binary, ConfMembID:16/little-unsigned, 16#f5:16/little-unsigned, 16#7f:8, 2:16/little-unsigned, ConfMembID:16/little-unsigned>>
               end,
    %%report_icp(result_to_action(Action, Result)),
    ips_response(Socket, MetaData, ?IPS_200_BG_A_IPCONFCONTROL_CNF, NCommand),
    {noreply, State};

handle_cast(Msg, State) ->
    info_manager:info("icp_server drop ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Bin}, #state{ socket = Sock
                                    , rest = Rest} = State) ->
    NRest = process_icp(<<Rest/binary,Bin/binary>>),
    NState = State#state{rest = NRest},
    {noreply, NState};
handle_info(heart, #state{socket = Sock} = State) ->
    Bin = <<8:16/little-unsigned
          , 16#10:8
          , 16#00:8
          , 16#01:8
          , 16#10:8
          , 16#00:8
          , 16#7F:8
          , 16#00:8
          , 16#00:8
          >>,
    %%info_manager:info("send heart beat~n"),
    case gen_tcp:send(Sock,Bin) of
        {error, Reason} ->
            info_manager:info("heartbeat exit by ~p~n",[Reason]),
	    info_manager:log_fatal("Core model error", "heartbeat exit"),
            {stop, Reason, State};
        _ ->
            report_icp(icp_heart_out),
            {noreply, State}
    end;
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    info_manager:info("icp server socket closed"),
    info_manager:log_error("Socket link broken",""),
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, #state{socket = Socket} = State) ->
    info_manager:info("icp server exit with socket: ~p~n",[Reason]),
    info_manager:log_fatal("Core model error", io_lib:format("~p: icp server exit with socket", [?MODULE])),
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
process_icp(Bin) when byte_size(Bin) < 2 ->
    Bin;
process_icp(<<Length:16/little-unsigned, _/binary>> = Bin)
                      when Length > (byte_size(Bin) - 2) ->
    Bin;
process_icp(<<8:16/little-unsigned,_:8/binary, Next/binary>>) ->
    %%info_manager:info("heartbeat receive"),
    report_icp(icp_heart_in),
    report_icp(icp_total),
    process_icp(Next);
process_icp(<<Length:16/little-unsigned,_/binary>> = Bin) ->
    TimerStart = os:timestamp(),
    <<L:16/little-unsigned,Content:Length/binary, Next/binary>> = Bin,
    info_manager:info("icp msg receive~n"),
    report_icp(icp_total),
    M = <<L:16/little-unsigned,Content/binary>>,
    {Tag, Msg} = parse_icp_head(M, TimerStart),
    case Tag of
        worker ->
            worker_pool:cast(Msg);
        conf ->
            info_manager:info("send to conf_pool ~n"),
            conf_pool:cast(Msg)
    end,
    process_icp(Next).

ips_response(Socket, MetaData, Operation, Command) ->
    gen_tcp:send(Socket ,gen_icp(MetaData, Operation, Command)).

parse_icp_head(Bin, TimerStart) ->
    %%解析时有可能出现长度不匹配等错误，暂不处理，不提供容错性
    %%info_manager:info("message receive~n"),

    %%<<_Length:16/little
    %%  Protocol_type:8/little,
    %%  Frame_type:8/little,
    %%  Sid:8/little,
    %%  Did:8/little,
    %%  Slot_num:8/little,
    %%  BPH:3/binary,
    %%  Frame/binary>> = Bin,

    <<_:16/little-unsigned
    , ICP_H:8/binary
    , Frame/binary>> = Bin,
    %%info_manager:info("frame length ~p~n",[byte_size(Frame)]),

    %%<<AreaId:32/little,
    %%  Host:32/little,
    %%  Sender:32/little,
    %%  Operation:32/little,
    %%  FrameBody/binary>> = Frame,

    <<FRAME_H:12/binary
    , Operation:32/little-unsigned
    , FrameBody/binary>> = Frame,
    %%info_manager:info("framebody length ~p~n",[byte_size(FrameBody)]),

    <<_BodyLength:32
    , Command/binary>> = FrameBody,
    info_manager:info("Command length ~p~n",[byte_size(Command)]),
    info_manager:info("Command is ~p~n",[Command]),

    <<RefNum:32/little-unsigned, ID:16/little-unsigned, _/binary>> = Command,
    %%info_manager:info("RefNum ~p~n",[RefNum]),
    MetaData = {TimerStart, ICP_H, FRAME_H},
    case Operation of
        ?IPS_200_BG_A_IVRSERVICE_REQ ->
            info_manager:info("get service ~p", [MetaData]),
            _IVRSlot = ID,
            <<COMMAND_H:8/binary, _/binary>> = Command,
            R = parse_service_command(Command),
            info_manager:info("get service command ~p", [R]),
            case R of
                [{set, _}|_] ->
                    gen_server:cast(self(), {alloc_media_ack, RefNum, {ack, ok}, {MetaData, COMMAND_H}}),
                    AckPara = ack_inst_to_para(#evs{}, R),
                    {worker, {alloc_media_ack, RefNum, AckPara, MetaData}};
                _ ->
                    PlayPara = play_inst_to_para(#media_ctrl{ type = audio
                                                            , barge = false }, R),
                    {worker, {play_media, RefNum, PlayPara, {rcs, {MetaData, COMMAND_H}, os:timestamp()}}}
            end;
        ?IPS_200_BG_A_MEDIARES_ALLOC_REQ ->
            %%<<RefNum:32
            %%, BoardSelector:16
            %%, TrunkMID:16
            %%, TrunkSLOT:16
            %%, SpecializedResourceType:16
            %%, BCType:8
            %%, AssocRsSlot:16
            %%, NbupMode,8
            %%, MediaAttribute:8
            %%, _Opt/binary>> = Command,
            report_icp(icp_alloc_total),
            AllocPara = { {port_need, 1}
                        , { {addr, inet, {10,1,70,180}}
                          , trans_audio(#media_descript{port = 4455, codeclist = [#codec{type = evs, payload = 96, ext = #ext{ align = oct, modeset = [8]}}]})
                          , none}},
            {worker, {alloc_media, RefNum, AllocPara, MetaData}};
        ?IPS_200_BG_A_IVRRELEASE_REQ ->
            report_icp(icp_release_total),
            _IVRSlot = ID,
            {worker, {release_media, RefNum, Command, MetaData}};
        ?IPS_200_BG_A_IPCONFCONTROL_REQ ->
            ConfRoomID = ID,
            Inst = parse_control_command(Command),
            case Inst of
                [{join, {ConfMembID, _IVRSlot, <<_AttributeFlag:16>>, COMMAND_H}}] ->
                    {conf, {join_conf, ConfRoomID, {member, ConfMembID}, {COMMAND_H,MetaData}}};
                [{unjoin, {ConfMembID, COMMAND_H}}] ->
                    {conf, {unjoin_conf, ConfRoomID, {member, ConfMembID}, {COMMAND_H, MetaData}}}
            end;
        ?IPS_200_BG_A_IPCONFCREATE_REQ ->
            %%<<RefNum:32
            %%, ConfReserveRes:16
            %%, ConfNature:8
            %%, VideoConfAttri:8
            %%, Reserve:32
            %%, AssocResource:5/binary>> = Command,
            report_icp(icp_create_total),
            {conf, {alloc_conf, {RefNum, new}, Command, {RefNum, MetaData}}};
        ?IPS_200_BG_A_IPCONFDELETE_REQ ->
            report_icp(icp_destroy_total),
            ConfRoomID = ID,
            {conf, {release_conf, {RefNum, ConfRoomID}, Command, {RefNum, MetaData}}}
    end.

parse_control_command(Command) ->
    %<<RefNum:32
    %, ConfRoomID:16
    %, SeqNumber:8
    %, OperationIndication:8
    %, Option/binary>> = Command,
    <<COMMAND_H:8/binary
    , Options/binary>> = Command,

    command_to_ins(control, COMMAND_H, Options).

parse_service_command(Command) ->
    %<<_:32
    %, IvrSlot:16
    %, SeqNum:8
    %, ServiceId:8
    %, Options/binary>> = Command,
    <<COMMAND_H:8/binary
    , Options/binary>> = Command,

    command_to_ins(service, COMMAND_H, Options).

command_to_ins(service, COMMAND_H, Options) ->
    <<_:7/binary, ServiceId:8>> = COMMAND_H,
    case ServiceId of
        16#05 ->   %ack,nbup
            [Option|_] = split_options(Options),
            lists:flatten(options_to_ins(COMMAND_H, Option, []));
        16#06 ->   %开始放音收号录音
            Command = case lists:flatten(options_to_ins(COMMAND_H, Options, [])) of
                [{dc, DCCommand}, {play, PlayCommand}] ->
                    report_icp(icp_pc_total),
                    lists:flatten([{dc_after_play}, DCCommand, PlayCommand]);
                [{play, PlayCommand}, {dc, DCCommand}] ->
                    report_icp(icp_pc_total),
                    lists:flatten([{dc_after_play}, DCCommand, PlayCommand]);
                [{dc, DCCommand}] ->
                    [{dc_only}|DCCommand];
                [{play, PlayCommand}] ->
                    [{play_only}|PlayCommand];
                [{rec, RecCommand}] ->
                    [{rec}|RecCommand]
            end,
            lists:flatten(Command);
        16#32 ->   %停止放音
            report_icp(icp_stop_play_total),
            [{stop, play, COMMAND_H}];
        16#33 ->   %停止收号
            report_icp(icp_stop_dc_total),
            [{stop, dc, COMMAND_H}];
        16#34 ->   %停止录音
            report_icp(icp_stop_rec_total),
            [{stop, rec, COMMAND_H}];
        16#0A ->
            report_icp(icp_stop_all_total),
            [{stop, all, COMMAND_H}];
        E ->
            error_logger:info_msg("command unknow ~p ~p~n~p~n",[E,COMMAND_H,Options]),
            []
    end;
command_to_ins(control, COMMAND_H, Options) ->
    <<_:7/binary, OperationId:8>> = COMMAND_H,
    case OperationId of
        16#05 ->  %会议成员属性修改
            lists:flatten(options_to_ins(COMMAND_H, Options, []));
        16#08 ->  %会议背景放音请求
            lists:flatten(options_to_ins(COMMAND_H, Options, []));
        16#09 ->  %会议录制请求
            lists:flatten(options_to_ins(COMMAND_H, Options, []));
        16#0C ->  %资源加入会议
            lists:flatten(options_to_ins(COMMAND_H, Options, []));
        16#0D ->  %资源退出会议
            lists:flatten(options_to_ins(COMMAND_H, Options, []));
        E ->
            error_logger:info_msg("command unknow ~p ~p~n~p~n",[E,COMMAND_H,Options]),
            []
    end.

options_to_ins(COMMAND_H, Options, EnvStack) ->
    lists:map(fun(Option) ->
        case Option of
            <<>> ->
                [];
            <<16#95:8,_/binary>> ->  %%ack
                report_icp(icp_ack_total),
                << _Id:8
                 , _Len:8
                 , IP1:8/unsigned
                 , IP2:8/unsigned
                 , IP3:8/unsigned
                 , IP4:8/unsigned
                 , Port:16/little-unsigned
                 , MediaAttribute:8/unsigned
                 , PayloadType:8
                 , _PacketTime:8
                 , _ConnInd:8
                 , DCModeCode:8
                 , PT_2833:8
                 , OptIPV6/binary>> = Option,  %%option后部sdp相关设定，暂时忽略

                IP = case {IP4,IP3,IP2,IP1} of
                    {0,0,0,0} ->
                        <<IP6_1:16/unsigned
                        , IP6_2:16/unsigned
                        , IP6_3:16/unsigned
                        , IP6_4:16/unsigned
                        , IP6_5:16/unsigned
                        , IP6_6:16/unsigned
                        , IP6_7:16/unsigned
                        , IP6_8:16/unsigned
                        , SubOpt/binary>> = OptIPV6,
                        {IP6_1,IP6_2,IP6_3,IP6_4,IP6_5,IP6_6,IP6_7,IP6_8};
                    _ ->
                        SubOpt = OptIPV6,
                        {IP4,IP3,IP2,IP1}
                end,
                MediaType = icp_type_trans(MediaAttribute),
                [
                    {set, {rtp, [ {client, {IP, Port}}
                                , {mediatype, MediaType}
                                , {payloadtype, PayloadType}
                                , {dc_mode, {DCModeCode, PT_2833}}]}}
                ,   {set, {play, {mediatype, MediaType}}}
                ,   {set, {play, {payloadtype, PayloadType}}}] ++
                lists:map(fun(Opt) ->
                             options_to_ins(COMMAND_H, Opt, [ack|EnvStack]) end,
                            split_options(SubOpt)) ++
                [   {ack, COMMAND_H} ];
            <<16#de:8, _/binary>> ->  %% evs
                << _Id:8
                 , _Len:8
                 , _HfOnly:8/unsigned
                 , _DtxOut:8/unsigned
                 , _DtxIn:8/unsigned
                 , _Cmr:8/unsigned
                 , ModeSwitch:8/unsigned
                 , EvsBitRate:8/unsigned
                 , _AMRWBIOModeSet:8/unsigned>> = Option,
                [
                      {set, {evs, {mode_switch, case ModeSwitch of 0 -> primary; _ -> io end}}}
                    , {set, {evs, {bitrate, EvsBitRate}}}
                ];

            <<16#69:8, _/binary>> ->
                case Option of
                    << _Id:8
                     , _OptLen:16/unsigned-little
                     , 1:8/unsigned
                     , StreamBW1:1/binary
                     , StreamBW0:1/binary
                     , _SampleChannel:2/binary>> -> %%amr mode set
                        << F:1/unsigned
                        , Index:15/unsigned>> = <<StreamBW0/binary, StreamBW1/binary>>,
                        case {F, Index} of
                           {0, _} ->
                               %%[ {set, {amr, {mode, max}}}];
                               %% 取默认值，所以不生成命令
                               [];
                           {1, Index} ->
                               [ {set, {amr, {mode, Index}}}]
                        end;
                    << _Id:8
                     , _OptLen:16/unsigned-little
                     , 2:8/unsigned
                     , _StreamBW1:1/binary
                     , _StreamBW0:1/binary
                     , _PR:8/unsigned
                     , FPS:8/unsigned>> -> %% video set
                        [{set, {video, {fps, FPS div 2}}}]
                end;

            <<16#6b:8, _/binary>> ->  %%amr字节对齐
                << _Id:8
                 , _OptLen:16/unsigned-little
                 , OctetAlign:8/unsigned >> = Option,
                 Align = case OctetAlign of
                    0 -> true;
                    1 -> false;
                    _ -> false
                end,
                [ {set, {amr, {align, Align}}}];

            <<16#b6:8, _/binary>> ->  %%多媒体播放
                report_icp(icp_pa_total),
                info_manager:info("option media play: ~p~n",[Option]),
                <<  _Id:8
                  , _OptLen:8/unsigned
                  , PlayTimes:8/unsigned
                  , InterVal:8/unsigned
                  , TotalTime:16/little-unsigned
                  , SubOpt/binary>> = Option,
                info_manager:info("Playtimes: ~p~nInterVal: ~p~nTotalTime: ~p~n",[PlayTimes, InterVal, TotalTime]),
                {play, [ {set, {play, {clean_file}}}
                       , {set, {play, {times, PlayTimes}}}
                       , {set, {play, {interval, InterVal}}}
                       , {set, {play, {totaltime, TotalTime}}}] ++
                lists:map(fun(Opt) ->
                             options_to_ins(COMMAND_H, Opt, [play|EnvStack]) end,
                            split_options(SubOpt)) ++
                        [ {play, COMMAND_H}]};
            <<16#b8:8, _/binary>> -> %%录音
                report_icp(icp_rec_total),
                info_manager:info("option media record: ~p~n", [Option]),
                <<  _Id:8
                  , _OptLen:8/unsigned
                  , RecordTime:16/little-unsigned
                  , _NoVoiceTime:8/unsigned
                  , _SilenceTime:8/unsigned
                  , _BeepControl:8/unsigned
                  , DTMFControl:8/unsigned
                  , RecordStopDgt:2/binary
                  , _RecordCancelDgt:2/binary
                  , _RecordPlayDgt:2/binary
                  , _RecordRestartDgt:2/binary
                  , SubOpt/binary>> = Option,
                info_manager:info("RecordTime: ~p~nDTMF: ~p~nRecordStopDgt: ~p~n",[RecordTime, DTMFControl, RecordStopDgt]),
                TermKey = case {DTMFControl, RecordStopDgt} of
                            {0, _} ->
                                none;
                            {2, <<1:8, _:4, Key:4>>} ->
                                Key;
                            _ ->
                                %%记错误日志
                                none
                        end,
                {rec, [ {set, {rec, {maxtime, RecordTime}}}
                      , {set, {rec, {termkey, icp_key_to_char(TermKey)}}}] ++
                lists:map(fun(Opt) ->
                             options_to_ins(COMMAND_H, Opt, [rec|EnvStack]) end,
                            split_options(SubOpt)) ++
                    [{rec, COMMAND_H}]};

            <<16#7a:8, _/binary>> ->    %% conf control background play
                info_manager:info("option ipconf background play: ~p~n", [Option]),
                <<  _Id:8
                  , _OptLen:16/little-unsigned
                  , AnnOrigin:8/unsigned
                  , MediaPlayCtlCmm:8/unsigned
                  , MediaVoiceOption:8/unsigned
                  , PlayTimes:8/unsigned
                  , SubOpt/binary>> = Option,
                info_manager:info("AnnOrigin: ~p~nMediaPlayCtlCmm: ~p~nMediaVoiceOption: ~p~nPlayTimes: ~p~n",[AnnOrigin, MediaPlayCtlCmm, MediaVoiceOption, PlayTimes]),
                case MediaPlayCtlCmm of
                    16#11 ->
                        report_icp(icp_conf_pa_total),
                        [{ media, play, COMMAND_H, [ {play_only}
                                                   , {set, {play, {mediatype, ?AUDIO_ULAW}}}
                                                   , {set, {play, {interval, 0}}}
                                                   , {set, {play, {totaltime, ?INFINITY}}}
                                                   , {set, {play, {times, PlayTimes}}} ] ++
                                                   lists:map(fun(Opt) ->
                                                                 options_to_ins(COMMAND_H, Opt, [play|EnvStack])
                                                             end, split_options(SubOpt)) ++
                                                   [{play, COMMAND_H}] }];
                    16#03 ->
                        report_icp(icp_conf_stop_pa_total),
                        [{stop_media, play, COMMAND_H, [{stop, play, COMMAND_H}]}];
                    _ ->
                        info_manager:info("Unsupport MediaPlayCtlCmm: ~p~n",[MediaPlayCtlCmm]),
                        []
                end;

            <<16#7b:8, _/binary>> ->    %% conf control rec
                info_manager:info("option ipconf record: ~p~n", [Option]),
                <<  _Id:8
                  , _OptLen:16/little-unsigned
                  , BCCmdType:8/unsigned
                  , _BCRecType:8/unsigned
                  , _BCRecFmt:8/unsigned
                  , RecTimeLen:16/little-unsigned
                  , RecFileSize:16/little-unsigned
                  , SubOpt/binary>> = Option,
                case BCCmdType of
                    16#01 ->
                        report_icp(icp_conf_rec_total),
                        info_manager:info("RecTimeLen: ~p~nRecFileSize: ~p~n",[RecTimeLen, RecFileSize]),
                        [{media, rec, COMMAND_H, [ {rec}
                                                 , {set, {rec, {maxtime, RecTimeLen}}} ] ++
                                                 lists:map(fun(Opt) ->
                                                               options_to_ins(COMMAND_H, Opt, [rec|EnvStack])
                                                           end, split_options(SubOpt)) ++
                                                 [{rec, COMMAND_H}]}];
                    16#03 ->
                        report_icp(icp_conf_stop_rec_total),
                        [{stop_media, rec, COMMAND_H, [{stop, rec, COMMAND_H}]}];
                    _ ->
                        info_manager:info("Unsupport BCCmdType: ~p~n",[BCCmdType]),
                        []
                end;

            <<16#c1:8,_/binary>> ->  %%conf rec file info
                info_manager:info("option conf record media info: ~p~n",[Option]),
                <<  _Id:8
                  , _OptLen:16/unsigned-little
                  , ServerIP:32/unsigned-little
                  , ServerPort:16/unsigned-little
                  , Filename/binary>> = Option,
                info_manager:info("ServerIP: ~p~nServerPort: ~p~nFilename: ~p~n",[ServerIP, ServerPort, Filename]),
                [0|RFilename] = lists:reverse(binary_to_list(Filename)),
                info_manager:info("Filename decode is: ~p~n",[lists:reverse(RFilename)]),
                [Env|_] = EnvStack,
                [{set, {Env, {file, lists:reverse(RFilename)}}}];

            <<16#c3:8,_/binary>> ->  %%conf audio record info
                info_manager:info("option audio record para: ~p~n",[Option]),
                [];

            <<16#83:8,_/binary>> ->  %%media info

                info_manager:info("option media info: ~p~n",[Option]),
                <<  _Id:8
                  , _OptLen:8/unsigned
                  , FileType:16/unsigned-little
                  , FileIP:32/unsigned-little
                  , FilePort:16/unsigned-little
                  , ForceLoad:8/unsigned
                  , Reserve:8/unsigned
                  , ReplaceAnn:32/unsigned-little
                  , Filename/binary>> = Option,
                info_manager:info("FileType: ~p~nFileIP: ~p~nFilePort: ~p~nForceLoad: ~p~nReserve: ~p~nReplaceAnn: ~p~nFilename: ~p~n",[FileType, FileIP, FilePort, ForceLoad, Reserve, ReplaceAnn, Filename]),
                [0|RFilename] = lists:reverse(binary_to_list(Filename)),
                info_manager:info("Filename decode is: ~p~n",[lists:reverse(RFilename)]),
                [Env|_] = EnvStack,
                [{set, {Env, {file, lists:reverse(RFilename)}}}];
            <<16#A1:8,_/binary>> -> %%语音播放参数列表
                info_manager:info("option sound para: ~p~n",[Option]),
                [];
            <<16#fb:8,_/binary>> -> %%录音文件模式
                info_manager:info("option record file: ~p~n",[Option]),
                [];
            <<16#fd:8,_/binary>> -> %%语音偏移量
                info_manager:info("option sound offset: ~p~n",[Option]),
                [];
            <<16#b7:8,_/binary>> -> %%收号
                report_icp(icp_dc_total),
                info_manager:info("option digit collect: ~p~n",[Option]),
                case Option of
                    << _Id:8
                    , 14:8
                    , BRK:8/unsigned
                    , MinCollect:8/unsigned
                    , MaxCollect:8/unsigned
                    , MaxTime:8/unsigned
                    , FDT:8/unsigned
                    , IDT:8/unsigned
                    , DigitCancel:16/unsigned-little
                    , DigitTerminate:16/unsigned-little
                    , DigitStart1:16/unsigned-little
                    , DigitStart2:16/unsigned-little>> ->

                        info_manager:info("BRK: ~p~nMinC: ~p~nMaxC: ~p~nMaxTime: ~p~nFDT: ~p~nIDT: ~p~nDCan: ~p~nDTerm: ~p~nDS1: ~p~nDS2: ~p~n",
                                  [BRK, MinCollect, MaxCollect, MaxTime, FDT, IDT, DigitCancel, DigitTerminate, DigitStart1,DigitStart2]),
                        DC_CONTROL = #dc_control{ brk = BRK
                                                , min_collect = MinCollect
                                                , max_collect = MaxCollect
                                                , max_time = MaxTime
                                                , fdt = FDT
                                                , idt = IDT
                                                , dgt_cancel = DigitCancel
                                                , dgt_terminate = DigitTerminate
                                                , dgt_start1 = DigitStart1
                                                , dgt_start2 = DigitStart2},
                        %% min_collect 可能比max_collect大，后续判断并返回参数错误
                        {dc, [{set, {dc, DC_CONTROL}}
                        ,{dc, COMMAND_H}]}
                end;

            <<16#7D:8, _/binary>> ->  %%加入会议
                << _Id:8
                 , _Len:16/little-unsigned
                 , IVRSlot:16/little-unsigned
                 , _ResType:8
                 , ConfMembID:16/little-unsigned
                 , _Type:8
                 , AttributeFlag:16/little-unsigned
                 , _VolumeInGain:8
                 , _VolumeOutGain:8
                 , _PayloadFor2833:8>> = Option,
                report_icp(icp_join_total),
                [{join, {ConfMembID, IVRSlot, <<AttributeFlag:16>>, COMMAND_H}}];
            <<16#7F:8, _/binary>> ->  %%退出会议
                << _Id:8
                 , _Len:8
                 , IVRSlot:16/little-unsigned>> = Option,
                report_icp(icp_unjoin_total),
                [{unjoin, {IVRSlot, COMMAND_H}}];
            <<16#76:8, _/binary>> ->                 %%修改成员属性
                << _Id:8
                 , _Len:16/little-unsigned
                 , ConfMembID:16/little-unsigned
                 , _Type:8
                 , AttributeFlag:16/little-unsigned
                 , _VolumeInGain:8
                 , _VolumeOutGain:8
                 , _PayloadFor2833:8>> = Option,
                 info_manager:info("Modify member, AttributeFlag: 0x~.16B~n", [AttributeFlag]),
                 [{modify, {ConfMembID, <<AttributeFlag:16>>, COMMAND_H}}]
        end

    end, split_options(Options)).

gen_frame(FRAME_H, Operation, Command) ->
    BodyLength = byte_size(Command),
    <<FRAME_H/binary
    , Operation:32/little-unsigned
    , BodyLength:32/little-unsigned
    , Command/binary>>.

gen_icp({_TimerStart, ICP_H, FRAME_H}, Operation, Command) ->
    Frame = gen_frame(FRAME_H, Operation, Command),
    Length = byte_size(Frame) + 8,
    <<Length:16/little-unsigned
    , ICP_H/binary
    , Frame/binary>>.

split_options(<<>>) ->
    [];

split_options(<<125:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<125:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

split_options(<<251:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<251:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

split_options(<<253:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<253:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

%% for 3.110 amr SmdStruct 0x69
split_options(<<105:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<105:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

%% for 3.112 amr octet-align 0x6b
split_options(<<107:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<107:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

%%for ip conf background play
split_options(<<122:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<122:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

%%for ip conf record
split_options(<<123:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<123:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

split_options(<<195:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<195:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

split_options(<<193:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<193:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

split_options(<<118:8/unsigned, L:16/little-unsigned, Content:L/binary, Rest/binary>>) ->
    Option = <<118:8/unsigned, L:16/little-unsigned, Content/binary>>,
    [Option|split_options(Rest)];

split_options(<<Id:8/unsigned, 130:8/unsigned, L:16/unsigned, Content:L/binary, Rest/binary>>) ->
    %%狗屁变长消息，好在读入后长度就没用了，为了兼容性，长度转意
    Option = <<Id:8/unsigned, 0:8/unsigned, Content/binary>>,
    [Option|split_options(Rest)];

split_options(Options) ->
    <<Id:8/unsigned, L:8/unsigned, Content:L/binary, Rest/binary>> = Options,
    Option = <<Id:8/unsigned, L:8, Content/binary>>,
    [Option|split_options(Rest)].

digits_to_binary(Digits) ->
    BinDigits = list_to_binary(Digits),
    case byte_size(BinDigits) of
        0 ->
            <<>>;
        DgtLen ->
            << 16#86:8
             , (DgtLen+2):8
             , 1:8
             , DgtLen:8
             , BinDigits/binary>>
    end.
icp_key_to_char(11) -> $*; %% icp star is not 10 but 11
icp_key_to_char(12) -> $#; %% icp sharp is not 11 but 12
%%icp_key_to_char(10) -> $?;
icp_key_to_char(N) when 0 =< N, N =< 9 -> $0 + N;
icp_key_to_char(none) -> none.

icp_type_trans(?MEDIA_ULAW)    -> {audio, ?AUDIO_ULAW};
icp_type_trans(?MEDIA_ALAW)    -> {audio, ?AUDIO_ALAW};
icp_type_trans(?MEDIA_AMRNB)   -> {audio, ?AUDIO_AMRNB};
icp_type_trans(?MEDIA_AMRWB)   -> {audio, ?AUDIO_AMRWB};
icp_type_trans(?MEDIA_EVS)   -> {audio, ?AUDIO_EVS};
icp_type_trans(?MEDIA_H264)    -> {video, ?VIDEO_H264}.

rec_result_to_bin({Reason, RecTime}) when Reason =:= maxtime
                                        ; Reason =:= termkey
                                        ; Reason =:= stopped ->
    R = case Reason of
            maxtime -> 1;
            termkey -> 2;
            stopped -> 5
        end,
    <<16#c6:8, 13:16/little-unsigned, -1:32/unsigned-little, RecTime:32/unsigned-little, -1:32/unsigned-little, R:8/unsigned>>;
rec_result_to_bin(_) ->
    <<>>.

%%report_icp函数名故意没有包含req，因为代码弹性，上层使用时
%%仅以icp的记录为目的，但实际使用时，因体系的不同可以选择上
%%报的内容。
report_icp(Action) when Action =:= icp_pc_total;
                        Action =:= icp_heart_in;
                        Action =:= icp_heart_out;
                        Action =:= icp_total ->
    info_manager:icp(Action);
report_icp(Action) ->
    info_manager:icp(Action),
    info_manager:req(icp_to_req(Action)).

icp_to_req(icp_alloc_total) -> req_alloc_total;
icp_to_req(icp_alloc_succ) -> req_alloc_succ;
icp_to_req(icp_alloc_fail) -> req_alloc_fail;

icp_to_req(icp_ack_total) -> req_confirm_total;
icp_to_req(icp_ack_succ) -> req_confirm_succ;
icp_to_req(icp_ack_fail) -> req_confirm_fail;

icp_to_req(icp_pa_total) -> req_play_total;
icp_to_req(icp_pa_succ) -> req_play_succ;
icp_to_req(icp_pa_fail) -> req_play_fail;

icp_to_req(icp_stop_play_total) -> req_stop_play_total;
icp_to_req(icp_stop_play_succ) -> req_stop_play_succ;
icp_to_req(icp_stop_play_fail) -> req_stop_play_fail;

icp_to_req(icp_dc_total) -> req_digit_collect_total;
icp_to_req(icp_dc_succ) -> req_digit_collect_succ;
icp_to_req(icp_dc_fail) -> req_digit_collect_fail;

icp_to_req(icp_stop_dc_total) -> req_stop_digit_collect_total;
icp_to_req(icp_stop_dc_succ) -> req_stop_digit_collect_succ;
icp_to_req(icp_stop_dc_fail) -> req_stop_digit_collect_fail;

icp_to_req(icp_rec_total) -> req_record_total;
icp_to_req(icp_rec_succ)  -> req_record_succ;
icp_to_req(icp_rec_fail)  -> req_record_fail;

icp_to_req(icp_stop_rec_total) -> req_stop_record_total;
icp_to_req(icp_stop_rec_succ)  -> req_stop_record_succ;
icp_to_req(icp_stop_rec_fail)  -> req_stop_record_fail;

icp_to_req(icp_stop_all_total) -> req_stop_all_total;
icp_to_req(icp_stop_all_succ)  -> req_stop_all_succ;
icp_to_req(icp_stop_all_fail)  -> req_stop_all_fail;

icp_to_req(icp_release_total) -> req_release_total;
icp_to_req(icp_release_succ) -> req_release_succ;
icp_to_req(icp_release_fail) -> req_release_fail;

icp_to_req(icp_create_total) -> req_create_total;
icp_to_req(icp_create_succ) -> req_create_succ;
icp_to_req(icp_create_fail) -> req_create_fail;

icp_to_req(icp_destroy_total) -> req_destroy_total;
icp_to_req(icp_destroy_succ) -> req_destroy_succ;
icp_to_req(icp_destroy_fail) -> req_destroy_fail;

icp_to_req(icp_join_total) -> req_join_total;
icp_to_req(icp_join_succ) -> req_join_succ;
icp_to_req(icp_join_fail) -> req_join_fail;

icp_to_req(icp_unjoin_total) -> req_unjoin_total;
icp_to_req(icp_unjoin_succ) -> req_unjoin_succ;
icp_to_req(icp_unjoin_fail) -> req_unjoin_fail;

icp_to_req(icp_conf_pa_total) -> req_conf_pa_total;
icp_to_req(icp_conf_pa_succ) -> req_conf_pa_succ;
icp_to_req(icp_conf_pa_fail) -> req_conf_pa_fail;

icp_to_req(icp_conf_stop_pa_total) -> req_conf_stop_pa_total;
icp_to_req(icp_conf_stop_pa_succ) -> req_conf_stop_pa_succ;
icp_to_req(icp_conf_stop_pa_fail) -> req_conf_stop_pa_fail;

icp_to_req(icp_conf_rec_total) -> req_conf_rec_total;
icp_to_req(icp_conf_rec_succ) -> req_conf_rec_succ;
icp_to_req(icp_conf_rec_fail) -> req_conf_rec_fail;

icp_to_req(icp_conf_stop_rec_total) -> req_conf_stop_rec_total;
icp_to_req(icp_conf_stop_rec_succ) -> req_conf_stop_rec_succ;
icp_to_req(icp_conf_stop_rec_fail) -> req_conf_stop_rec_fail.

result_to_action(play, {ok, _, _}) ->
    icp_pa_succ;
result_to_action(play, {error, _}) ->
    icp_pa_fail;
result_to_action(stop, {ok, play}) ->
    icp_stop_play_succ;

result_to_action(dc, {ok, _, _}) ->
    icp_dc_succ;
result_to_action(dc, {error, _, _}) ->
    icp_dc_fail;
result_to_action(stop, {ok, dc}) ->
    icp_stop_dc_succ;

result_to_action(rec, {ok, _}) ->
    icp_rec_succ;
result_to_action(rec, {error, _}) ->
    icp_rec_fail;
result_to_action(stop, {ok, rec}) ->
    icp_stop_rec_succ;

result_to_action(stop, {ok, all}) ->
    icp_stop_all_succ;

result_to_action(join, ok) ->
    icp_join_succ;
result_to_action(unjoin, ok) ->
    icp_unjoin_succ;
result_to_action(join, {error, _}) ->
    icp_join_fail;
result_to_action(unjoin, {error, _}) ->
    icp_unjoin_fail;

result_to_action(conf_stop, {ok, play}) ->
    icp_conf_stop_pa_succ;
result_to_action(conf_stop, {ok, rec}) ->
    icp_conf_stop_rec_succ;
result_to_action(conf_stop, {{error, _}, play}) ->
    icp_conf_stop_pa_fail;
result_to_action(conf_stop, {{error, _}, rec}) ->
    icp_conf_stop_rec_fail.

trans_audio(#media_descript{port = Port, codeclist = [Codec|_]} = Desc) when Port =/= 0 ->
    Digit = #digit_desc{ type = rfc2833
               , payload = 99},
    case Codec of
        #codec{type = Type, payload = Payload, ext = Ext} when Type =:= amrnb
                                                             ; Type =:= amrwb ->
            #ext{ align = Align, modeset = ModeSet} = Ext,
            OctAlign = case Align of
                           eff ->
                               false;
                           oct ->
                               true
                       end,
            #audio_desc{ port = Port
                       , codec = codec_trans(Type)
                       , payload = Payload
                       , amr_ext = {lists:max(ModeSet), OctAlign}
                       , digit = Digit};
        #codec{type = Type, payload = Payload} ->
            #audio_desc{ port = Port
                       , codec = codec_trans(Type)
                       , payload = Payload
                       , digit = Digit}
    end;
trans_audio(#media_descript{}) ->
    none.

trans_digit(#media_descript{digit = []}) ->
    #digit_desc{type = dtmf};
trans_digit(#media_descript{digit = [Digit|_]}) ->
    #digit_desc{ type = rfc2833
               , payload = Digit#codec.payload}.

codec_trans(ulaw) ->
    ?AUDIO_ULAW;
codec_trans(alaw) ->
    ?AUDIO_ALAW;
codec_trans(amrnb) ->
    ?AUDIO_AMRNB;
codec_trans(amrwb) ->
    ?AUDIO_AMRWB;
codec_trans(evs) ->
    ?AUDIO_EVS;
codec_trans(h263) ->
    ?VIDEO_H263;
codec_trans(h264) ->
    ?VIDEO_H264.

ack_inst_to_para(EVS, [{set, {rtp, Inst}} |T]) ->
    {client, {IP, Port}} = lists:keyfind(client, 1, Inst),
    {mediatype, {audio, AudioType}} = lists:keyfind(mediatype, 1, Inst),
    {payloadtype, Payload} = lists:keyfind(payloadtype, 1, Inst),
    ack_inst_to_para(EVS#evs{ addr = IP
                            , port = Port
                            , type = AudioType
                            , payload = Payload}
                      , T);
ack_inst_to_para(EVS, [{set, {evs, {mode_switch, S}}} |T]) ->
    ack_inst_to_para(EVS#evs{evs_mode_switch = S}, T);
ack_inst_to_para(EVS, [{set, {evs, {bitrate, R}}} |T]) ->
    ack_inst_to_para(EVS#evs{evs_bitrate = R}, T);
ack_inst_to_para(EVS, [_ |T]) ->
    ack_inst_to_para(EVS, T);
ack_inst_to_para(EVS, []) ->
    IPVer = inet,
    #evs{ addr = IP
        , port = Port
        , type = AudioType
        , payload = Payload
        , evs_mode_switch = Switch
        , evs_bitrate = Bitrate} = EVS,
    Digit = #digit_desc{ type = rfc2833
                       , payload = 99},
    AudioDescript = #audio_desc{ port = Port
                               , codec = codec_trans(AudioType)
                               , payload = Payload
                               , digit = Digit
                               , evs_ext = {Switch, Bitrate}},
    { {port_need, 1}
    , { {addr, IPVer, IP}
      , AudioDescript
      , none}}.


play_inst_to_para(Para, [{set, {play, {clean_file}}}|T]) ->
    play_inst_to_para(Para, T);
play_inst_to_para(Para, [{set, {play, {times, N}}}|T]) ->
    play_inst_to_para(Para#media_ctrl{times = N}, T);
play_inst_to_para(Para, [{set, {play, {interval, N}}}|T]) ->
    play_inst_to_para(Para#media_ctrl{interval = N}, T);
play_inst_to_para(Para, [{set, {play, {totaltime, N}}}|T]) ->
    play_inst_to_para(Para#media_ctrl{max_time = N*1000}, T);
play_inst_to_para(Para, [{set, {play, {file, F}}}|T]) ->
    NFilenames = Para#media_ctrl.filenames ++ [F],
    play_inst_to_para(Para#media_ctrl{filenames = NFilenames}, T);
play_inst_to_para(Para, [_|T]) ->
    play_inst_to_para(Para, T);
play_inst_to_para(Para, []) ->
    Para.
