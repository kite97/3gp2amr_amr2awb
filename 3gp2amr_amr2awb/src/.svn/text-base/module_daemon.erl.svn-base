%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@mint>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2013 by wanglihe <wanglihe@mint>
%%%-------------------------------------------------------------------
-module(module_daemon).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(delay_time, 900*1000).

-record(state, { module
               , type}).

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
start_link(Module, Type, Param) ->
    gen_server:start_link(?MODULE, [Module, Type, Param], []).

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
init([Module, Type, Param]) when Type =:= core; Type =:= important; Type =:= common ->
    process_flag(trap_exit, true),
    info_manager:log_info("Model init", io_lib:format("~p starting", [Module])),
    case catch erlang:apply(Module, start_link, Param) of
        {ok, _Pid} ->
            info_manager:log_info("Model init ok", io_lib:format("~p init ok", [Module])),
            {ok, #state{ module = Module, type = Type }};
        {error, Reason} when Type =:= core ->
            error_logger:info_msg("~p init error: ~p~n", [Module, Reason]),
            info_manager:log_fatal("Model init fail", io_lib:format("~p init fail", [Module])),
            info_manager:log_fatal("Process start fail", "imp start fail"),
            init:stop(),
            {stop, normal};
        {error, Reason} when Type =:= important ->
            error_logger:info_msg("~p init error: ~p~n", [Module, Reason]),
            info_manager:log_fatal("Model init fail", io_lib:format("~p init fail", [Module])),
            erlang:start_timer(0, self(), shutdown),
            {ok, #state{module = none, type = none}};
        {error, Reason} when Type =:= common ->
            error_logger:info_msg("~p init error: ~p~n", [Module, Reason]),
            info_manager:log_fatal("Model init fail", io_lib:format("~p init fail", [Module])),
            erlang:start_timer(?delay_time, self(), shutdown),
            {ok, #state{module = none, type = none}};
        _ ->
            %%此处错误应该不仅仅是模块内部原因，所以将信息打全
            info_manager:log_fatal("Model init fail", io_lib:format("~p init fail with ~p, ~p", [Module, Type, Param])),
            info_manager:log_fatal("Process start fail", "imp start fail"),
            init:stop()
    end;
init([Module, Type, Param]) ->
    %%此处错误应该不仅仅是模块内部原因，所以将信息打全
    info_manager:log_fatal("Model init fail", io_lib:format("~p init fail with ~p, ~p", [Module, Type, Param])),
    info_manager:log_fatal("Process start fail", "imp start fail"),
    init:stop().

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
    {noreply, State}.

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
%%module分为三类：core,important,common
%%core module(ips_emulator.erl等)初始化失败，进程退出；运行中异常退出，立即重启
%%important module(worker_pool.erl等)初始化失败或运行中异常退出，立即重启
%%common module(rings_cache.erl等)初始化失败或运行中异常退出，延迟重启
handle_info({'EXIT', _Pid, Reason}, #state{module = Module, type = core} = State) ->
    error_logger:info_msg("~p error reason: ~p~n", [Module, Reason]),
    info_manager:log_fatal("Core model error", io_lib:format("~p exit", [Module])),
    {stop, normal, State};
handle_info({'EXIT', _Pid, Reason}, #state{module = Module, type = important} = State) ->
    error_logger:info_msg("~p error reason: ~p~n", [Module, Reason]),
    info_manager:log_fatal("Important model error", io_lib:format("~p exit", [Module])),
    {stop, normal, State};
handle_info({'EXIT', _Pid, Reason}, #state{module = Module, type = common} = State) ->
    error_logger:info_msg("~p error reason: ~p~n", [Module, Reason]),
    info_manager:log_error("Common model error", io_lib:format("~p exit", [Module])),
    erlang:start_timer(?delay_time, self(), shutdown),
    {noreply, State};
handle_info({timeout, _, Reason}, State) ->
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
