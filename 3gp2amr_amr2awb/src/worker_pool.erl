%%%-------------------------------------------------------------------
%%% File    : worker_pool.erl
%%% Author  :  <wanglihe@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created : 27 Dec 2012 by  <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(worker_pool).

-behaviour(gen_server).

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%-export([register/1, find/1, cast/1]).
-export([cast/1]).

-define(SERVER, ?MODULE).
-define(MAX_CAPS, 250).

-record(state, { ssrcs
               , ports
               , worker_remain
               , local_ip
               , amr_trans
	       , maxtime}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(WorkerMax, LocalIP, PortBase, SSRCBase, AmrTrans, WorkerMaxTime) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [WorkerMax, LocalIP, PortBase, SSRCBase, AmrTrans, WorkerMaxTime], []).

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
init([WorkerMax, LocalIP, PortBase, SSRCBase, AmrTrans, WorkerMaxTime]) ->
    process_flag(trap_exit, true),
    %%process_flag(priority, high),
    ets:new(?SERVER, [set, named_table, protected]),
    info_manager:info("new worker pool ~n"),
    Ports = [P||P <- lists:seq(PortBase, PortBase + (WorkerMax*4)  + (?MAX_CAPS*4) - 1), P rem 2 =:= 0],
    info_manager:worker({reset, WorkerMax}),
    SSRCS = lists:seq(SSRCBase, SSRCBase+WorkerMax*2+(?MAX_CAPS*2)-1),
    {ok, #state{ ports = queue:from_list(Ports)
               , worker_remain = WorkerMax
               , ssrcs = SSRCS
               , local_ip = LocalIP
               , amr_trans = AmrTrans
	       , maxtime = WorkerMaxTime}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({find, Key}, _From, State) ->
    case ets:lookup(?SERVER, Key) of
        [] ->
            {reply, {error, noexist}, State};
        [{_, Pid}] ->
            {reply, {ok, Pid}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({Commander, { alloc_media
                        , CallID
                        , _AllocPara
                        , MetaData}} = _Alloc
                        , #state{worker_remain = Remain} = State) when Remain =< 0 ->
    info_manager:session(session_total),
    gen_server:cast(Commander, {alloc_media, CallID, {error, eworkermax}, MetaData}),
    info_manager:log_fatal("No available resource", ""), %%当超过workmax会出现的很频繁
    info_manager:session(session_fail),
    {noreply, State};
handle_cast({Commander, { alloc_media
                        , CallID
                        , {{port_need, PortNeed}, _} = _AllocPara, MetaData}} = Alloc
                        , #state{worker_remain = Remain} = State) ->

    info_manager:session(session_total),
    case ets:lookup(?SERVER, {Commander, CallID}) of
        [] ->
            {PortUse, NPorts} = firstn(State#state.ports, PortNeed, []),
            {SSRCUse, NSSRC} = firstn(State#state.ssrcs, PortNeed, []),
            case {PortUse, SSRCUse} of
                {_, []} ->
                    gen_server:cast(Commander, {alloc_media, CallID, {error, eworkermax}, MetaData}),
                    info_manager:log_fatal("No available resource", ""), %%当超过workmax会出现的很频繁
                    info_manager:session(session_fail),
                    {noreply, State};
                _ ->
                    info_manager:info("borrow socket: ~p ssrc: ~p~n", [PortUse, SSRCUse]),
                    case worker:start_link(SSRCUse, State#state.local_ip, PortUse, State#state.amr_trans,State#state.maxtime) of
                        {ok, Pid} ->
                            put(Pid, {{Commander, CallID}, PortUse, SSRCUse}),
                            ets:insert(?SERVER, {{Commander, CallID}, Pid}),
                            info_manager:info("alloc over ~n"),
                            worker:cast(Pid, Alloc),
                            info_manager:worker(start),
                            NState = State#state{ ports = NPorts
                                                , ssrcs = NSSRC
                                                , worker_remain = Remain - 1},
                            %%有可能worker的初始化化简，就省了时间，这里回了
                            %%gen_server:cast({SrcPid, {alloc, CallID, {ok, Port}}, MetaData}),
                            {noreply, NState};
                        _ ->
                            %%再议错误类型
                            gen_server:cast(Commander, {alloc_media, CallID, {error, eworkermax}, MetaData}),
                            info_manager:session(session_fail),
                            {noreply, State}
                    end
            end;
        _ ->
            gen_server:cast(Commander, {alloc_media, CallID, {error, eworkerexist}, MetaData}),
            {noreply, State}
    end;
handle_cast({Commander, {ControlTag , CallID, _, MetaData}} = Control, State) when ControlTag =:= alloc_media_ack
                                                                                 ; ControlTag =:= play_media
                                                                                 ; ControlTag =:= stop_pa
                                                                                 ; ControlTag =:= stop_pc
                                                                                 ; ControlTag =:= stop_pr
                                                                                 ; ControlTag =:= digit_collect
                                                                                 ; ControlTag =:= record_media
                                                                                 ->
    case ets:lookup(?SERVER, {Commander, CallID}) of
        [] ->
            gen_server:cast(Commander, {ControlTag, CallID, {error, exist}, MetaData}),
            {noreply, State};
        [{_, Pid}] ->
            worker:cast(Pid, Control),
            {noreply, State}
    end;

handle_cast({Commander, {release_media, CallID, _, MetaData}}, State) ->
    case ets:lookup(?SERVER, {Commander, CallID}) of
        [] ->
            gen_server:cast(Commander, {release_media, CallID, {error, exist}, MetaData}),
            {noreply, State};
        [{_, Pid}] ->
            %%这里也许可以套用爹模式直接向worker发信号，同时削减worker的相关代码
            worker:cast(Pid, release),
            gen_server:cast(Commander, {release_media, CallID, ok, MetaData}),
            {noreply, State}
    end;

handle_cast({Commander, {realloc_media, CallID, AllocPara, MetaData}}, State) ->
    ets:delete(?SERVER, {Commander, CallID}),
    handle_cast({Commander, {alloc_media, CallID, AllocPara, MetaData}}, State);

handle_cast(Msg, State) ->
    info_manager:info("~p get unknow msg ~p", [?MODULE, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when is_pid(Pid) ->
    case erase(Pid) of
        undefined ->
            {noreply, State};
        {{Commander, CallID}, OldPort, OldSSRC} ->
            case Reason of
                  %%除了与端口有关的，资源可以正常回收
                _ when Reason =/= {shutdown, ebadport}
                     , Reason =/= {shutdown, close_bad_port} ->

                    case Reason of
                        normal ->
                            info_manager:session(session_succ);
                        {shutdown, worker_timeout} ->
		            info_manager:log_warn("Session time out", io_lib:format("{~p} time out", [{CallID}])),
                            info_manager:session(session_fail);
                        _ ->
                            info_manager:session(session_fail)
                    end,
                    ets:delete(?SERVER, {Commander, CallID}),
                    info_manager:worker(stop),
                    Ports = insertq(State#state.ports, OldPort),
                    Remain = State#state.worker_remain + 1,
                    SSRCS = OldSSRC ++ State#state.ssrcs,
                    NState = State#state{ ports = Ports
                                        , ssrcs = SSRCS
                                        , worker_remain = Remain},
                    info_manager:info("return port: ~p ssrc: ~p~n", [OldPort, OldSSRC]),
                    {noreply, NState};
                {shutdown, ebadport} ->
                    %% 如果是video会一次干掉俩，哪怕其中一个是正常的，以后再改
                    ets:delete(?SERVER, {Commander, CallID}),
                    info_manager:log_info("Bad port number", io_lib:format("~p for refnum ~p", [OldPort, CallID])),
                    error_logger:info_msg("bad port number: ~p, with reason ~p~n",[OldPort, Reason]),
                    info_manager:worker(stop),
                    {noreply, State};
                {shutdown, close_bad_port} ->
	            %%info_manager:log_info("Bad port number", io_lib:format("~p", [Port])),
                    error_logger:info_msg("bad port number: ~p, with reason ~p~n",[OldPort, Reason]),
                    info_manager:worker(stop),
                    %%close失败怎么算呢，再议
                    %%info_manager:session(session_fail);
                    {noreply, State}
            end
    end;
handle_info({'EXIT', Pid, Reason}, State) when is_port(Pid) ->
    %%port原则上是不会异常退出的，现在还没想好如何处理，有需求时
    %%再加入操作
    error_logger:info_msg("port ~p exit with ~p", [Pid, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:foldl(fun(_, _) -> info_manager:session(session_fail) end, ok, ?SERVER),
    info_manager:worker({reset, 0}).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------

cast(Msg) ->
    catch gen_server:cast(?SERVER, {self(), Msg}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
firstn(L, 0, R) ->
    {lists:reverse(R), L};
firstn(L, N, R) when is_list(L) ->
    case L of
        [] ->
            {[], L};
        [V|NL]  ->
            firstn(NL, N-1, [V|R])
    end;
firstn(Q, N, R) ->
    case  queue:out(Q) of
        {empty, _} ->
            {[], Q};
        {{value, V}, NQ} ->
            firstn(NQ, N-1, [V|R])
    end.

insertq(Q, []) ->
    Q;
insertq(Q, [H|T]) ->
    insertq(queue:in(H, Q), T).
