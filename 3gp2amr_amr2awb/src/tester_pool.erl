%%%-------------------------------------------------------------------
%%% @author lijinyu <lijinyu@mint>
%%% @copyright (C) 2014, lijinyu
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2014 by lijinyu <lijinyu@mint>
%%%-------------------------------------------------------------------
-module(tester_pool).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([start_test/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([cast/1]).

-define(SERVER, ?MODULE).

-define(PORT_BASE, 10000).
-define(WORKER_MAX, 5000).
-define(SSRC_BASE, 20000).

-record(state, { ports
               , refnum = 0
               , ssrcs
               , local_ip
               , fake_local_ip}).

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
start_link(LocalIP, FakeLocalIP) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [LocalIP, FakeLocalIP], []).

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
init([LocalIP, FakeLocalIP]) ->
    process_flag(trap_exit, true),
    ets:new(?SERVER, [set, named_table, protected]),
    Ports = [P||P <- lists:seq(?PORT_BASE, ?PORT_BASE + ?WORKER_MAX*2 - 1), P rem 2 =:= 0],
    SSRCS = lists:seq(?SSRC_BASE, ?SSRC_BASE + ?WORKER_MAX - 1),
    {ok, LIP} = inet:parse_address(LocalIP),
    {ok, FIP} = inet:parse_address(FakeLocalIP),
    {ok, #state{ ports = Ports
               , ssrcs = SSRCS
               , local_ip = LIP
               , fake_local_ip = FIP}}.

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
handle_call({new, Mod}, _From,  #state{ refnum = RefNum
                                        , fake_local_ip = FIP
                                        , local_ip = IP} = State) ->
    info_manager:info("get mod ~p", [Mod]),
    [Port|Ports] = State#state.ports,
    [SSRC|SSRCS] = State#state.ssrcs,
    case catch Mod:start_link(RefNum, IP, Port, SSRC, FIP) of
        {ok, Pid} ->
            ets:insert(?SERVER, {RefNum, Pid}),
            put(Pid, {RefNum, Port, SSRC}),
            %%info_manager:info("get pid ~p,ref ~p", [Pid, RefNum]),
            {reply, {ok, RefNum}, State#state{ ports = Ports
                                 , refnum = RefNum + 1
                                 , ssrcs = SSRCS}};
        _ ->
            info_manager:info("bad ~p ~p ~p", [RefNum, Port, SSRC]),
            {reply, {error, bad_port}, State#state{ ports = Ports
                                 , refnum = RefNum + 1
                                 , ssrcs = SSRCS}}
    end;
handle_call({new_join, JoinParam, Mod}, _From,  #state{ refnum = RefNum
                                        , fake_local_ip = FIP
                                        , local_ip = IP} = State) ->
    info_manager:info("get mod ~p", [Mod]),
    [Port|Ports] = State#state.ports,
    [SSRC|SSRCS] = State#state.ssrcs,
    case catch Mod:start_link(RefNum, IP, Port, SSRC, FIP, JoinParam) of
        {ok, Pid} ->
            ets:insert(?SERVER, {RefNum, Pid}),
            put(Pid, {RefNum, Port, SSRC}),
            %%info_manager:info("get pid ~p,ref ~p", [Pid, RefNum]),
            {reply, {ok, RefNum}, State#state{ ports = Ports
                                 , refnum = RefNum + 1
                                 , ssrcs = SSRCS}};
        _ ->
            info_manager:info("bad ~p ~p ~p", [RefNum, Port, SSRC]),
            {reply, {error, bad_port}, State#state{ ports = Ports
                                 , refnum = RefNum + 1
                                 , ssrcs = SSRCS}}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast({test, {Mod, Para}}, #state{ refnum = RefNum
                                        , fake_local_ip = FIP
                                        , local_ip = IP} = State) ->
    info_manager:info("get mod ~p, ~p", [Mod, Para]),
    [Port|Ports] = State#state.ports,
    [SSRC|SSRCS] = State#state.ssrcs,
    case catch Mod:start_link(RefNum, IP, Port, SSRC, FIP) of
        {ok, Pid} ->
            ets:insert(?SERVER, {RefNum, Pid}),
            put(Pid, {RefNum, Port, SSRC}),
            %%info_manager:info("get pid ~p,ref ~p", [Pid, RefNum]),
            {noreply, State#state{ ports = Ports
                                 , refnum = RefNum + 1
                                 , ssrcs = SSRCS}};
        _ ->
            info_manager:info("bad ~p ~p ~p", [RefNum, Port, SSRC]),
            {noreply, State#state{ ports = Ports
                                 , refnum = RefNum + 1
                                 , ssrcs = SSRCS}}
    end;

handle_cast({_Command, _Result, {RefNum, _IvrSlot}, _AddrPort} = ICP, State) ->
    case ets:lookup(?SERVER, RefNum) of
        [] ->
            {noreply, State};
        [{_, Pid}] ->
            gen_server:cast(Pid, ICP),
            {noreply, State}
    end;

handle_cast({_Command, _Result, {RefNum, _}} = ICP, State) ->
    case ets:lookup(?SERVER, RefNum) of
        [] ->
            {noreply, State};
        [{_, Pid}] ->
            gen_server:cast(Pid, ICP),
            {noreply, State}
    end;

handle_cast({_Command, _Result, RefNum} = ICP, State) ->
    case ets:lookup(?SERVER, RefNum) of
        [] ->
            {noreply, State};
        [{_, Pid}] ->
            gen_server:cast(Pid, ICP),
            {noreply, State}
    end;

handle_cast(Msg, State) ->
    info_manager:info("unknown msg: ~p", [Msg]),
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
handle_info({'EXIT', Pid, Reason}, State) when is_pid(Pid) ->
    case erase(Pid) of
        undefined ->
            {noreply, State};
        {RefNum, Port, SSRC} when Reason =:= bad ->
            info_manager:info("bad ~p ~p ~p", [RefNum, Port, SSRC]),
            {noreply, State};
        {RefNum, Port, SSRC} ->
            ets:delete(?SERVER, RefNum),
            Ports = [Port|State#state.ports],
            SSRCS = [SSRC|State#state.ssrcs],
            NState = State#state{ ports = Ports
                                , ssrcs = SSRCS},
            {noreply, NState}
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
terminate(_Reason, _State) ->
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
cast(Msg) ->
    gen_server:cast(?SERVER, Msg).
start_test(Pid, Mod) ->
    info_manager:info("new seq 2"),
    gen_server:cast(Pid, {test, Mod}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
