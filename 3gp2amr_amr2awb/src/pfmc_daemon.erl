%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@mint>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2013 by wanglihe <wanglihe@mint>
%%%-------------------------------------------------------------------
-module(pfmc_daemon).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, cast/1]).

-define(SERVER, ?MODULE).

-include("../include/status.hrl").

-define(default_pulse_time, 1000).
-define(default_check_time, 5000).
%% 15分钟间隔，整15分记录
-define(default_record_time, 15).

-record(state, { port
               , pulse_timer
               , check_timer
               , pfmc_timer
               , last_response_time
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
    process_flag(trap_exit, true),
    %%open_port以及后续操作失败会抛异常，不用抓了，反正也是直接退出
    Command = "../pfmc/pfmc",
    info_manager:info("command is: ~p~n",[Command]),
    case lists:keyfind(pfmc, 1, Configs) of
        {pfmc, {PfmcIP, PfmcPort}} ->
            info_manager:info("pfmc ip: ~p port: ~p",[PfmcIP, PfmcPort]),
            Port = open_port({spawn_executable, Command}, [ use_stdio
                                                          , stderr_to_stdout
                                                          , stream
                                                          , {args,[ os:getpid()
                                                                  , PfmcIP
                                                                  , integer_to_list(PfmcPort)]}]),
            {ok, PulseTimer} = timer:send_interval(?default_pulse_time, pulse),
            {ok, CheckTimer} = timer:send_interval(?default_check_time, check),
            {Timeout, StopTime} = next_stop(os:timestamp()),
            PfmcTimer = erlang:start_timer(Timeout, self(), {pfmc, StopTime}),
            time_clear(?TIMEKEYS),
            ets:new(?SERVER, [set, named_table, protected]),
            {ok, #state{ port = Port
                       , pulse_timer = PulseTimer
                       , check_timer = CheckTimer
                       , pfmc_timer = PfmcTimer
                       , last_response_time = os:timestamp()}};
        false ->
            {stop, pfmcconfig}
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
handle_cast({rtp, {Addr, Direct}, {Count, Size}}, State) ->
    %%info_manager:info("~p ~p", [{Addr, Direct}, Size]),
    case ets:lookup(?SERVER, {Addr, Direct}) of
        [{_, [RtpCount, TotalSize, TotalLostCount]}] when Direct =:= send ->
            ets:insert(?SERVER, {{Addr, Direct}, [RtpCount+Count, TotalSize+Size, TotalLostCount]});
        [] when Direct =:= send ->
            ets:insert(?SERVER, {{Addr, Direct}, [Count, Size, 0]});

        [{_, [ RtpCount
             , TotalSize
             , TotalLostCount
             , LostPktSum
             , LostOctetSum
             , DelaySum
             , DelayMax
             , ShakeSum
             , ShakeMax
             , DelayAvg
             , ShakeAvg]}] when Direct =:= recv ->
            Delay = random:uniform(10),
            Shake = random:uniform(20),
            ets:insert(?SERVER, {{Addr, Direct}, [ RtpCount+Count
                                                 , TotalSize+Size
                                                 , TotalLostCount
                                                 , LostPktSum
                                                 , LostOctetSum
                                                 , DelaySum + Delay
                                                 , erlang:max(DelayMax, Delay)
                                                 , ShakeSum + Shake
                                                 , erlang:max(ShakeMax, Shake)
                                                 , (DelayAvg+Delay) div 2
                                                 , (ShakeAvg+Shake) div 2]});
        [] when Direct =:= recv ->
            LostPkt = 0,
            LostPktSum = 0,
            LostOctetSum = 0,
            DelaySum = 0,
            DelayMax = 0,
            ShakeSum = 0,
            ShakeMax = 0,
            DelayAvg = 0,
            ShakeAvg = 0,
            ets:insert(?SERVER, {{Addr, Direct}, [ Count
                                                 , Size
                                                 , LostPkt
                                                 , LostPktSum
                                                 , LostOctetSum
                                                 , DelaySum
                                                 , DelayMax
                                                 , ShakeSum
                                                 , ShakeMax
                                                 , DelayAvg
                                                 , ShakeAvg]})
    end,
    {noreply, State};
handle_cast({Key, Start, Stop}, State) ->
    case get(Key) of
        {Count, Amount} ->
            put(Key, {Count+1, Amount+timer:now_diff(Stop, Start)}),
            {noreply, State};
        undefined ->
            {noreply, State}
    end;
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
handle_info(pulse, #state{port = Port} = State) ->
    port_command(Port, <<"P">>),
    {noreply, State};
handle_info({Port, {data, _Data}}, #state{port = Port} = State) ->
    NState = State#state{last_response_time = os:timestamp()},
    {noreply, NState};
handle_info(check, #state{last_response_time = Time} = State) ->
    case timer:now_diff(os:timestamp(), Time) of
        D when D >= ?default_check_time*1000 ->
            info_manager:log_error("pfmc daemon exit", "pfmc no response"),
            {stop, pfmc_no_response, State};
        _ ->
            {noreply, State}
    end;
handle_info({timeout, PfmcTimer, {pfmc, StopTime}}
            , #state{ port = Port
                    , pfmc_timer = PfmcTimer} = State) ->
    refresh_status(?TIMEKEYS),
    StopDateTime = calendar:now_to_local_time(StopTime),
    {{Year,Month,Day},{Hour,Min,Second}} = StopDateTime,
    DateTimeString = "R" ++
        io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B"
                     ,[Year,Month,Day,Hour,Min,Second]),
    port_command(Port, list_to_binary(DateTimeString)),

    time_clear(?TIMEKEYS),
    rtp_clear(),
    %%由于超时计时对毫秒取整，所以实际上是短于所需的时间的，
    %%也就是说，收到消息时，马上做next_stop，有可能得到的结果是同
    %%样的时间。放到处理过后，降低出现此问题的概率。其实精确
    %%计时的抖动问题，至使收取到事件的时间前后均可，没有方法
    %%可以避免。next_stop没有做多一毫秒的时间修正，原因是没有
    %%必要浪费计算时间，和加入一行代码。
    {Timeout, NStopTime} = next_stop(os:timestamp()),
    NPfmcTimer = erlang:start_timer(Timeout, self(), {pfmc, NStopTime}),
    {noreply, State#state{pfmc_timer = NPfmcTimer}};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    info_manager:log_error("log daemon exit", "pfmc exit"),
    {stop, Reason, State};
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
terminate(_Reason, #state{port = Port}) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            catch port_close(Port),
            catch os:cmd(io_lib:format("kill -9 ~p", [OsPid]));
        _ ->
            ok
    end.

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

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
time_clear(Keys) ->
    lists:foreach(fun(Key) -> put(Key, {0, 0}) end, Keys).
rtp_clear() ->
    ets:delete_all_objects(?SERVER).
refresh_status(Keys) ->
    status:report_rtp(ets:tab2list(?SERVER)),
    status:report_time(lists:map(fun(Key)->
                    case get(Key) of
                        {0, 0} ->
                            {Key, 0.0};
                        {Count, Amount} ->
                            {Key, (Amount/Count)/1000}
                    end end, Keys)).
next_stop(NowTime) ->
    {MegaSecs, Secs, _} = NowTime,
    {Date, {H,M,_}} = calendar:now_to_universal_time({MegaSecs, Secs + (?default_record_time*60), 0}),
    StopDateTime = {Date, {H, M - (M rem ?default_record_time), 0}},
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    Seconds = calendar:datetime_to_gregorian_seconds(StopDateTime) - 62167219200,
    StopTime = {Seconds div 1000000, Seconds rem 1000000, 0},
    TimeDiff = timer:now_diff(StopTime, NowTime),
    {TimeDiff div 1000, StopTime}.
