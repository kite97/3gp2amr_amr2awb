%%%-------------------------------------------------------------------
%%% @author lijinyu <lijinyu@mint>
%%% @copyright (C) 2013, lijinyu
%%% @doc
%%%
%%% @end
%%% Created : 20 Nov 2013 by lijinyu <lijinyu@mint>
%%%-------------------------------------------------------------------
-module(ocarina_tester).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/0, set/2, cast/2]).

-export([send_icp/1]).

-define(SERVER, ?MODULE).

-define(heart_timeout, 3000).

-define(REF_BASE, 10000).

-record(state, {socket
               , test_mod
              , pulse
              , refnum_base
              , worker_num
              , worker_max
              , caps
              , curtime
              , maxtime
              , parent
              , server
              , rest = <<>>}).

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
start_link(IP, Port, TestMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [IP, Port, TestMod], []).

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
init([IP, Port, TestMod]) ->
    process_flag(trap_exit, true),
    SocketICP = case gen_tcp:connect(IP, Port, [binary, {packet, 0}]) of
       {ok, Sock} ->
          Sock;
       {error, R} ->
          exit(R)
    end,
    {ok, _HeartTimer} = timer:send_interval(?heart_timeout, heart),
    %%{ok, _CapsTimer} = timer:send_interval(1000, caps),
    {ok, _CapsTimer} = timer:send_after(10, caps),
    {ok, #state{ socket = SocketICP
               , test_mod = TestMod
               , refnum_base = ?REF_BASE}}.

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
handle_cast({icp, Msg}, #state{socket = Sock} = State) ->
    case gen_tcp:send(Sock, Msg) of
        {error, Reason} ->
            {stop, Reason, State};
        _ ->
            %%info_manager:info("send icp succ"),
            {noreply, State}
    end;
handle_cast(Msg, State) ->
    info_manager:info("other msg: ~p", [Msg]),
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
handle_info({tcp, _Sock, Bin}, #state{rest = Rest} = State) ->
    NRest = process_icp(<<Rest/binary,Bin/binary>>),
    NState = State#state{rest = NRest},
    {noreply, NState};
handle_info(caps, State) ->
    info_manager:info("new caps"),
    TestMod = State#state.test_mod,
    lists:foreach(fun(_) ->
                info_manager:info("new seq"),
                tester_pool:start_test(tester_pool, {TestMod,[{total, 1}]}) end,
                lists:seq(1,10)),
    {noreply, State};
handle_info(heart, #state{socket = Sock} = State) ->
    Bin = <<8:16/little
          , 16#10:8
          , 16#00:8
          , 16#01:8
          , 16#10:8
          , 16#00:8
          , 16#7F:8
          , 16#00:8
          , 16#00:8
          >>,
    case gen_tcp:send(Sock, Bin) of
        {error, Reason} ->
            {stop, Reason, State};
        _ ->
            {noreply, State}
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
terminate(Reason, State) ->
    gen_tcp:close(State#state.socket),
    info_manager:info("~p get ~p, stop", [?MODULE, Reason]),
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
%%start_test(Pid, Type, Param, Caps, Duration, LocalIP) ->
start() ->
    RemoteIP = "10.1.70.180",
    %%RemoteIP = "10.1.69.107",
    %%RemoteIP = "10.1.69.51",
    RemotePort = 7477,
    LocalIP = "10.1.93.184",
    %%LocalIP = "10.1.70.181",
    %%FakeLocalIP = "10.1.70.181", %%对方发包去往地址
    FakeLocalIP = LocalIP, %%对方发包去往地址
    TestMod = test_conf,
    ?MODULE:start_link(RemoteIP, RemotePort, TestMod),
    pulse_server:start_link(),
    {ok, _PoolPid} = tester_pool:start_link(LocalIP, FakeLocalIP).

set(Pid, Msg) ->
    gen_server:call(Pid, Msg).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

process_icp(Bin) when byte_size(Bin) < 2 ->
    Bin;
process_icp(<<Length:16/little, _/binary>> = Bin)
                      when Length > (byte_size(Bin) - 2) ->
    Bin;
process_icp(<<8:16/little,_:8/binary, Next/binary>>) ->
    process_icp(Next);
process_icp(<<Length:16/little,_/binary>> = Bin) ->
    <<L:16/little, Content:Length/binary, Next/binary>> = Bin,
    M = <<L:16/little, Content/binary>>,
    case catch icp:decode(M) of
        {'EXIT', _} ->
            info_manager:info("~p", [Bin]),
            timer:sleep(100000),
            erlang:halt(),
            Bin;
        Result ->
            tester_pool:cast(Result),
            process_icp(Next)
    end.

send_icp(Msg) ->
    gen_server:cast(?SERVER, {icp, Msg}).
