%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2013 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(status).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([report/1, report_time/1, report_rtp/1, test/0, get_status/0]).

-define(SERVER, ?MODULE).
-define(default_timeout, 1000).

-include("status.hrl").

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
start_link(Configs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Configs], []).

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
init([Configs]) ->
%    error_logger:info_msg("~p start~n", [?MODULE]),
    lists:foreach(fun dict_int_initial/1, ?REQKEYS++?SESSIONKEYS++?WORKERKEYS++?CONFKEYS),
    lists:foreach(fun dict_float_initial/1, ?TIMEKEYS),
    LocalIP = case lists:keyfind(ipv4, 1, Configs) of
        {ipv4, IPV4} ->
            IPV4;
        false ->
            case lists:keyfind(ipv6, 1, Configs) of
                {ipv6, IPV6} ->
                    IPV6;
                false ->
                    none
            end
    end,
    put(lip, LocalIP),
    lists:foreach(fun rtp_key_initial/1, ?RTPKEYS),
    put(kpi_send, []),
    put(kpi_recv, []),
    case timer:send_interval(?default_timeout, status) of
        {ok, _TRef} when LocalIP =/= none ->
            {ok, #status{ start_time = os:timestamp()
                       , version = version:get_version()}};
        {ok, _} ->
            {stop, localip};
        {error, Reason} ->
            {stop, Reason}
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
handle_call({get, status}, _From, State) ->
    %%info_manager:info("status: ~p", [State]),
    {reply, State, State};
handle_call({report_time, TimeList}, _From, State) ->
    lists:foreach(fun({Key, Value}) ->
                {_,Format} = get(Key),
                put(Key, {Value, Format}) end, TimeList),
    handle_info(status, State),
    {reply, ok, State};
handle_call({report_rtp, RtpList}, _From, State) ->
    {TotalSendCount, TotalSendBytes} =
        lists:foldl(fun({{_,send}, [Cnt, Byte|_]}, {C, B}) ->
                            {C+Cnt, B+Byte};
                       (_, {C, B}) ->
                            {C, B}
                    end, {0,0}, RtpList),
    {TotalRecvCount, TotalRecvBytes} =
        lists:foldl(fun({{_,recv}, [Cnt, Byte|_]}, {C, B}) ->
                            {C+Cnt, B+Byte};
                       (_, {C, B}) ->
                            {C, B}
                    end, {0,0}, RtpList),

    lists:foreach(fun({Key, Value}) ->
                {_,Format} = get(Key),
                put(Key, {Value, Format}) end, [ {rtp_send_sendpkts, TotalSendCount}
                                               , {rtp_send_sendbytes, TotalSendBytes}
                                               , {rtp_recv_rcvpkts, TotalRecvCount}
                                               , {rtp_recv_rcvbytes, TotalRecvBytes}
                                               ]),
    put(kpi_send, lists:filter(fun({{_, send}, _}) -> true;
                                  (_) -> false
                               end, RtpList)),
    put(kpi_recv, lists:filter(fun({{_, recv}, _}) -> true;
                                  (_) -> false
                               end, RtpList)),
    handle_info(status, State),
    {reply, ok, State};
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
handle_cast({Tag, Key}, State) when Tag =:= session;
                                    Tag =:= req ->
    {Num, Format} = get(Key),
    put(Key, {(Num + 1) band 16#7FFFFFFF, Format}),
    {noreply, State};

handle_cast({worker, start}, #status{ worker_running = WorkerRunning
                                   , worker_peak = WorkerPeak} = State) ->
    NWorkerRunning = WorkerRunning + 1,
    {noreply, State#status{ worker_running = NWorkerRunning
                         , worker_peak = max(NWorkerRunning, WorkerPeak)}};
handle_cast({worker, stop}, #status{worker_running = WorkerRunning} = State) ->
    {noreply, State#status{worker_running = WorkerRunning - 1}};
handle_cast({worker, blocked}, #status{worker_blocked = WorkerBlocked} = State) ->
    %%目前worker_pool收到alloc确认为一个session开始。但实际上应该分为两
    %%个不同的概念分别增加。暂时只有端口占用触发了此问题，所以简单的处
    %%理为减一，当有更多此类问题时，应该重新设计session计数关口。
    {Num, Format} = get(session_total),
    put(session_total, {(Num - 1) band 16#7FFFFFFF, Format}),
    {noreply, State#status{worker_blocked = WorkerBlocked + 1}};

handle_cast({worker, {reset, Max}}, State) ->
    info_manager:info("status reset worker max: ~p", [Max]),
    {_,WorkerMaxFormat} = get(worker_max),
    put(worker_max, {Max, WorkerMaxFormat}),
    {noreply, State#status{ worker_max = Max
                          , worker_running = 0
                          , worker_blocked = 0}};

handle_cast({conf, start}, #status{ conf_running = ConfRunning
                                   , conf_peak = ConfPeak} = State) ->
    NConfRunning = ConfRunning + 1,
    {noreply, State#status{ conf_running = NConfRunning
                         , conf_peak = max(NConfRunning, ConfPeak)}};
handle_cast({conf, stop}, #status{conf_running = ConfRunning} = State) ->
    {noreply, State#status{conf_running = ConfRunning - 1}};

handle_cast({conf, {reset, Max}}, State) ->
    {_,ConfMaxFormat} = get(conf_max),
    put(conf_max, {Max, ConfMaxFormat}),
    {noreply, State#status{conf_max = Max, conf_running = 0}};

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
handle_info(status, #status{ worker_running = WorkerRunning
                          , worker_peak = WorkerPeak
                          , worker_blocked = WorkerBlocked
                          , conf_running = ConfRunning
                          , conf_peak = ConfPeak } = State) ->
    Start = State#status.start_time,
    Seconds = timer:now_diff(os:timestamp(), Start) div 1000000,
    {Days, {H,M,S}} = calendar:seconds_to_daystime(Seconds),

    {SessionTotal,_} = get(session_total),
    SessionCaps = trunc((SessionTotal - State#status.session_last_total)*(1000/?default_timeout)),
    {_,SessionCapsFormat} = get(session_caps),
    put(session_caps, {SessionCaps, SessionCapsFormat}),

    {_,WorkerRunningFormat} = get(worker_running),
    put(worker_running, {WorkerRunning, WorkerRunningFormat}),
    {_,WorkerPeakFormat} = get(worker_peak),
    put(worker_peak, {WorkerPeak, WorkerPeakFormat}),
    {_,WorkerBlockedFormat} = get(worker_blocked),
    put(worker_blocked, {WorkerBlocked, WorkerBlockedFormat}),

    {_,ConfRunningFormat} = get(conf_running),
    put(conf_running, {ConfRunning, ConfRunningFormat}),
    {_,ConfPeakFormat} = get(conf_peak),
    put(conf_peak, {ConfPeak, ConfPeakFormat}),

    {WorkerMax,_} = get(worker_max),
    WorkerIdle = WorkerMax - WorkerRunning - WorkerBlocked,
    {_,WorkerIdleFormat} = get(worker_idle),
    put(worker_idle, {WorkerIdle, WorkerIdleFormat}),

    {ConfMax,_} = get(conf_max),
    ConfIdle = ConfMax - ConfRunning,
    {_,ConfIdleFormat} = get(conf_idle),
    put(conf_idle, {ConfIdle, ConfIdleFormat}),

    Status = io_lib:format("Uptime: ~p days, ~p:~p:~p~n", [Days, H, M, S])
             ++ io_lib:format("Version: ~s~n",[State#status.version])
             ++ lists:foldr(fun(Key, Lines) ->
                            {Num, Format} = get(Key),
                            io_lib:format(Format, [Num]) ++ Lines
                         end,
                         "",
                         ?REQKEYS++?SESSIONKEYS++?WORKERKEYS++?CONFKEYS++?TIMEKEYS ++ ?RTPKEYS)
             ++ gen_kpi(?KPISENDKEYS, get(kpi_send))
             ++ gen_kpi(?KPIRECVKEYS, get(kpi_recv)),

%    catch file:write_file("/dev/shm/ocarina_"++os:getpid()++".status", Status)
    erlang:spawn(fun () ->
        case file:write_file("/dev/shm/imp_"++os:getpid()++".status", Status) of
            ok -> none;
            {error, _Reason} -> info_manager:log_error("File operation fail", "write status to shm failed")
        end
    end),
    {noreply, State#status{session_last_total = SessionTotal}};
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
%% Convert process status when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

report(M) ->
    gen_server:cast(?MODULE, M).
report_time(TimeList) ->
    gen_server:call(?MODULE, {report_time, TimeList}).
report_rtp(RtpList) ->
    gen_server:call(?MODULE, {report_rtp, RtpList}).

test() ->
    {ok, Pid} = ?SERVER:start_link(),
    info_manager:add_handler(worker_event),
    info_manager:add_handler(conf_event),
    info_manager:add_handler(session_event),
    info_manager:add_handler(req_event),
    lists:foreach(fun(Key) ->
                info_manager:req(Key) end, ?REQKEYS),
    lists:foreach(fun(Key) ->
                info_manager:worker(Key) end, ?WORKERKEYS),
    lists:foreach(fun(Key) ->
                info_manager:conf(Key) end, ?CONFKEYS),
    lists:foreach(fun(Key) ->
                info_manager:session(Key) end, ?SESSIONKEYS),
    Pid.
get_status() ->
    gen_server:call(?SERVER, {get, status}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
dict_int_initial(Key) ->
    put(Key, {0, atom_to_format(Key, "~p")}).

dict_float_initial(Key) ->
    put(Key, {0.0, atom_to_format(Key, "~p")}).

rtp_key_initial(Key) when Key =:= rtp_send_lip
                        ; Key =:= rtp_recv_lip ->
    LocalIP = get(lip),
    put(Key, {LocalIP, atom_to_format(Key, "~s")});
rtp_key_initial(Key) ->
    put(Key, {0, atom_to_format(Key, "~p")}).


atom_to_format(Key, Format) ->
    string:join(lists:map(fun capitalize/1
                         , string:tokens(atom_to_list(Key),"_"))," ")
    ++ ": "
    ++ Format
    ++ "~n".
capitalize([Cap|T]) ->
    [string:to_upper(Cap)|T].

gen_kpi(Keys, StatusData) ->
    Formats = lists:map(fun(Key) when Key =:= kpi_send_lip
                                    ; Key =:= kpi_send_rip
                                    ; Key =:= kpi_recv_lip
                                    ; Key =:= kpi_recv_rip ->
                                atom_to_format(Key, "~s");
                           (Key) ->
                                atom_to_format(Key, "~p") end, Keys),
    lists:foldl(fun({{RemoteIP,_}, Data}, Lines) ->
                    LocalIP = get(lip),
                    lists:foldr(fun({F,A}, L) ->
                                    io_lib:format(F, [A]) ++ L
                                end, Lines, lists:zip(Formats, [LocalIP, inet:ntoa(RemoteIP)] ++ Data))
                end, "", StatusData).
