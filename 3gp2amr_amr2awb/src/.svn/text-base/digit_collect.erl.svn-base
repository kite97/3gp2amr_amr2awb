%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ocarina.dev.ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  4 Jan 2013 by  <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(digit_collect).

-behaviour(gen_server).

%% API
-export([start_link/4, cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("media_desc.hrl").

-record(state, { worker
               , timer
               , number = none
               , pattern = none
               , data_transfer
               , max_timer = none
               , digit_timer = none
               , ctl = none
               , msg_para
               , buffer = []
               , time_start
               , src = none
               , error = none}).

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
-spec start_link(Trans, DCPara, Src, StartTimer) -> term() when
      Trans :: term(),
      DCPara :: term(),
      Src :: client | mixer,
      StartTimer :: term().
start_link(Trans, DCPara, Src, StartTimer) ->
    gen_server:start_link(?MODULE, [self(), Trans, DCPara, Src, StartTimer], []).

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
init([Worker, Trans, DCPara, Src, StartTimer]) ->
    process_flag(trap_exit,true),
    info_manager:info("~p ~p init~n",[?MODULE, self()]),
    %%应用于整个收号的maxtimer目前暂时不实现，因为msml中并无相关参数，但为与rps
    %%兼容，未来可能需加放默认值或是设计系统配置参数

    Timer = DCPara#digit_ctrl.timer,
    DigitTimer = case Timer#digit_timer.start_fdt_timer of
        true ->
            gen_fdttimer(Timer#digit_timer.fdt);
        _ ->
            none
    end,
    {DT, _, _, _, _} = Trans,
    data_transfer:start_service(DT, {listen, {digit, Src}}),
    info_manager:time(time_digit_collect_delay, StartTimer),
    case DCPara#digit_ctrl.digit of
        Dgt when is_record(Dgt, digit_dgt) ->
            {ok, #state{ worker = Worker
                       , data_transfer = DT
                       , timer = Timer
                       , number = Dgt
                       , ctl = DCPara
                       , time_start = os:timestamp()
                       , digit_timer = DigitTimer
                       , src = Src}};
        Pattern when is_record(Pattern, digit_pattern) ->
            ErlangPattern = lists:flatmap(fun mgcp_to_re/1, Pattern#digit_pattern.pattern),
            info_manager:info("ErlangPattern: ~p", [ErlangPattern]),
            case re:compile(ErlangPattern) of
                {ok, CompiledPattern} ->
                    {ok, #state{ worker = Worker
                               , data_transfer = DT
                               , timer = Timer
                               , pattern = CompiledPattern
                               , ctl = DCPara
                               , time_start = os:timestamp()
                               , digit_timer = DigitTimer
                               , src = Src}};
                {error, Reason} ->
                    %%这里讨论一下要不要日志
                    info_manager:info("pattern error: ~p", [Reason]),
                    exit({shutdown, epattern})
            end
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
handle_cast({digit, Digit}, #state{ worker = Worker
                                  , src = mixer} = State) ->
    #digit_ctrl{barge = Barge} = State#state.ctl,
    case Barge of
        true ->
            gen_server:cast(Worker, digit_collect_barge);
        false ->
            gen_server:cast(Worker, {digit_collect_stop, {ok, match, [Digit]}})
    end,
    {stop, normal, State};

handle_cast({digit, Digit}, #state{ worker = Worker
                                  , number = Dgt
                                  , timer = Timer} = State) ->
    info_manager:info("digit received is ~p~n",[Digit]),
    cancel_timer(State#state.digit_timer),
    NBuffer = [Digit|State#state.buffer],
    #digit_timer{ fdt = Fdt
                , idt = Idt} = Timer,
    #digit_ctrl{barge = Barge} = State#state.ctl,
    case Barge of
        true ->
            worker:cast(State#state.worker, digit_collect_barge);
        _ ->
            ok
    end,
    case check_termination(NBuffer, State#state.pattern, State#state.number) of
        {true, Reason} when Reason =:= termkey -> %%rfc 2897 eik not in the digit
            #digit_dgt{ min = Min
                      , max = Max} = Dgt,
            Digits = lists:reverse(State#state.buffer),
            case digits_len_check(Min, Max, Digits) of
                ok ->
                    worker:cast(Worker, {digit_collect_stop, {ok, match, Digits}}),
                    {stop, normal, State};
                WrongLenReason ->
                    worker:cast(Worker, {digit_collect_stop, {ok, WrongLenReason, Digits}}),
                    {stop, normal, State}
            end;
        {true, Reason} ->
            Digits = lists:reverse(NBuffer),
            worker:cast(Worker, {digit_collect_stop, {ok, Reason, Digits}}),
            {stop, normal, State};
        {false, normal} ->
            DigitTimer = gen_idttimer(Idt),
            NState= State#state{ buffer = NBuffer
                               , digit_timer = DigitTimer},
            {noreply, NState};
        {false, cancel} ->
            DigitTimer = gen_fdttimer(Fdt),
            {noreply, State#state{ buffer = []
                                 , digit_timer = DigitTimer}}
    end;

handle_cast(start_fdt, #state{digit_timer = none , timer = Timer} = State) ->
    info_manager:info("start fdt timer"),
    #digit_timer{fdt = Fdt} = Timer,
    DigitTimer = gen_fdttimer(Fdt),
    {noreply, State#state{digit_timer = DigitTimer}};

handle_cast(stop, #state{ worker = Worker } = State) ->
    Buffer = State#state.buffer,
    Digits = lists:reverse(Buffer),
    %%{IVRSlot, MetaData, COMMAND_H} = MsgPara,
    worker:cast(Worker, {digit_collect_stop, {ok, stopped, Digits}}),
    %%worker:cast(State#state.worker, {dc, over}),
    {stop, normal, State};

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
handle_info({timeout, TimerRef, _Msg}, #state{ worker = Worker, buffer = []} = State)
                                when TimerRef =:= State#state.digit_timer ->
    worker:cast(Worker, {digit_collect_stop, {ok, noinput, []}}),
    {stop, normal, State};
handle_info({timeout, TimerRef, _Msg}, #state{ worker = Worker
                                             , number = none
                                             , pattern = Pattern} = State)
        when TimerRef =:= State#state.digit_timer ->
    info_manager:info("digit collect timeout, check for normal stop~n"),
    Digits = lists:reverse(State#state.buffer),
    case re:run(Digits, Pattern, [{capture, none}]) of
        match ->
            worker:cast(Worker, {digit_collect_stop, {ok, match, Digits}}),
            {stop, normal, State};
        _ ->
            worker:cast(Worker, {digit_collect_stop, {ok, nomatch, Digits}}),
            {stop, normal, State}
    end;
handle_info({timeout, TimerRef, _Msg}, #state{ worker = Worker
                                             , number = Dgt
                                             , pattern = none} = State)
        when TimerRef =:= State#state.digit_timer ->
    #digit_dgt{ min = Min
              , max = Max} = Dgt,
    info_manager:info("digit collect timeout, check for normal stop~n"),
    Buffer = State#state.buffer,
    Digits = lists:reverse(Buffer),
    case digits_len_check(Min, Max, Digits) of
        ok ->
            worker:cast(Worker, {digit_collect_stop, {ok, match, Digits}}),
            {stop, normal, State};
        Reason ->
            worker:cast(Worker, {digit_collect_stop, {ok, Reason, Digits}}),
            info_manager:log_debug("Digit collect error", io_lib:format("~p", [Reason])),
            {stop, normal, State}
    end;
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
terminate(_Reason, #state{ time_start = StartTimer} = _State) ->
    info_manager:time(time_digit_collecting, StartTimer),
    %%worker:cast(Worker, {dc, over}),
    info_manager:info("~p ~p terminate",[?MODULE, self()]),
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

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_termination(_Buffer, _Pattern, none) ->
    {false, normal};
check_termination([Digit|_], none, Number) ->
    #digit_dgt{ rik = Rik
              , eik = Eik} = Number,
    case Digit of
        Eik ->
            {true, termkey};
        Rik ->
            {false, cancel};
        _ ->
            {false, normal}
    end.

digits_len_check(Min, _Max, Digits) when Min =/= none, length(Digits) < Min ->
    wrong_length;
digits_len_check(_Min, Max, Digits) when Max =/= none, Max < length(Digits) ->
    wrong_length;
digits_len_check(_Min, _Max, _Digits) ->
    ok.

cancel_timer(infinity) ->
    ok;
cancel_timer(none) ->
    ok;
cancel_timer(TimerRef) ->
    case erlang:cancel_timer(TimerRef) of
        false ->
            %%消息已经发出，以后决定要做何处理
            %%现在通过判断TimerRef是否相等进行处理
            %%其实，cancel timer目的在于减少系统资源占用
            ok;
        _Time ->
            ok
    end.

gen_fdttimer(Timeout) ->
    info_manager:info("dc first timeout is ~p~n",[Timeout]),
    gen_timer(Timeout, digittime_reach).

gen_idttimer(Timeout) ->
    info_manager:info("dc next timeout is ~p~n",[Timeout]),
    gen_timer(Timeout, digittime_reach).

gen_timer(infinity, _) ->
    infinity;
gen_timer(Timeout, Msg) ->
    erlang:start_timer(Timeout, self(), Msg).

%%mgcp and megaco基本是一致的，但是rcs不作区分下发，我就是想分开支持也没办法
%%所以现在实现为相同的变换
%%RFC 2705 3435 --> mgcp
%%RFC 3015 3525 --> megaco
%%megaco 还有个5125，没细看，看看起来好像是废掉了，Exx和Fxx这种匹配方式很挫，很没意
%%思，废了就废了吧。
mgcp_to_re(X) when X =:= $x
                 ; X =:= $X->
    "[0-9]";
mgcp_to_re($.) ->
    [$*];
mgcp_to_re($*) ->
    [$\\, $*];
mgcp_to_re(F) when F =:= $f
                 ; F =:= $F->
    [$#];
mgcp_to_re(E) when E =:= $e
                 ; E =:= $E->
    [$\\, $*];
mgcp_to_re(C) ->
    [C].
