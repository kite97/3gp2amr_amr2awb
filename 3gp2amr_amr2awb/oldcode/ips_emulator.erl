%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ocarina.dev.ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 14 Jan 2013 by wanglihe <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(ips_emulator).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state, { lsocket
               , lip
               , lport

               , log_file

               , accept = none
               , ipv4 = none
               , ipv6 = none
               , pfmc_addr_port = none
               , worker_max
               , port_base
               , ssrc_base
	       , worker_max_time = infinity}).

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
    process_flag(trap_exit,true),
    case lists:foldl(fun(Config, State) ->
                            catch configure(Config, State)
                        end
                        , #state{}
                        , Configs) of
        {error, Reason} ->
            info_manager:log_error("Config error", io_lib:format("~p",[Reason])),
            error_logger:info_msg("ips_emulator start error ~p~n", [Reason]),
            {stop, Reason};
        EState when EState#state.ipv4 =:= none
                  , EState#state.ipv6 =:= none ->
            Reason = no_ip_addr,
            info_manager:log_error("Config error", io_lib:format("~p",[Reason])),
            error_logger:info_msg("ips_emulator start error ~p~n", [Reason]),
            {stop, Reason};

        EState when EState#state.pfmc_addr_port =:= none ->
            Reason = no_pfmc,
            info_manager:log_error("Config error", io_lib:format("~p",[Reason])),
            error_logger:info_msg("ips_emulator start error ~p~n", [Reason]),
            {stop, Reason};

        State ->
            error_logger:tty(false),
            error_logger:logfile({open, State#state.log_file}),

            info_manager:add_handler(icp_event),
            info_manager:add_handler(worker_event),
            info_manager:add_handler(conf_event),
            info_manager:add_handler(session_event),
            info_manager:add_handler(req_event),

            ListenPort = State#state.lport,
            case gen_tcp:listen( ListenPort
                                  , [ binary
                                    , {packet,0}
                                    , {reuseaddr, true}
                                    , {active,false}]) of
                {ok, LSocket} ->
                    Accept = proc_lib:spawn_link(fun() ->
                                        accept_loop(LSocket) end),
                    NState = State#state{lsocket = LSocket
                                        ,accept = Accept},
                    error_logger:info_msg("ips_emulator start~n"),
                    case file:write_file("ocarina.pid", os:getpid()) of
                        ok ->
                            info_manager:log_info("Process start ok", "imp start ok"),
                            {ok, NState};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    error_logger:info_msg("ips_emulator listen error ~p~n", [Reason]),
                    %%module init fail??  Core model error??
                    info_manager:log_error("Socket listen fail", "ips_emulator listen error"),
                    {stop, Reason}
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
handle_call({accept, Sock, PeerName}, _From, State) ->
    error_logger:info_msg("got accept sock~n"),
    error_logger:info_msg("receive new sock ~p~n",[Sock]),
    LocalIP = {State#state.ipv4, State#state.ipv6},
    WorkerMax = State#state.worker_max,
    PortBase = State#state.port_base,
    SSRCBase = State#state.ssrc_base,
    WorkerMaxTime = State#state.worker_max_time,
    %%如果不成功，emulator会退出，各种退出，以后告警
    case icp_server_sup:start_link( Sock
                                  , LocalIP
                                  , WorkerMax
                                  , PortBase
                                  , SSRCBase
				  , WorkerMaxTime) of
        {ok, _} ->
            {reply, {ok}, State};
        {error, {already_started, _}} ->
            error_logger:info_msg("icp is running already, close"),
            info_manager:log_error("Socket connection refuse", io_lib:format("~p",[PeerName])),
            catch gen_tcp:close(Sock),
            {reply, {ok}, State};
        {error, {shutdown, _} = Reason} ->
            error_logger:info_msg("subprocess init error: ~p~n", [Reason]),
            {reply, {ok}, State}
    end;
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
handle_info({'EXIT', Pid, Reason}, State)
        when Pid =:= State#state.accept ->
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
terminate(_Reason, State) ->
    gen_tcp:close(State#state.lsocket),
    error_logger:info_msg("close lsocket~n"),
    error_logger:info_msg("ips_emulator exit~n"),
    info_manager:log_info("Process exit", "imp stopped").

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

accept_loop(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Sock} ->
            error_logger:info_msg("new socket accepted~n"),
            case inet:peername(Sock) of
                {ok, PeerName} ->
                    info_manager:log_info("Socket connection ok", io_lib:format("~p",[PeerName])),
                    unlink(Sock),
                    gen_server:call(?MODULE, {accept, Sock, PeerName});
                Result ->
                    info_manager:log_error("Socket connection refuse", io_lib:format("~p",[Result])),
                    catch gen_tcp:close(Sock)
            end,
            accept_loop(LSocket);
        {error,Reason} ->
            %%未来实现告警
            exit({accept,Reason})
    end.

configure(_, {error, _} = E) ->
    E;
configure({lport, {Port}}, State) ->
    State#state{lport = Port};
configure({worker_max, {Max}}, State) ->
    State#state{worker_max= Max};
configure({port_base, {Base}}, State) ->
    State#state{port_base = Base};
configure({ssrc_base, {Base}}, State) ->
    State#state{ssrc_base = Base};

%%ipv4和ipv6任一有错，则配置错误
configure({ipv4, IP}, State) ->
    case inet:parse_ipv4_address(IP) of
        {ok, IPV4} ->
            case is_match_local(IPV4) of
                true ->
                    State#state{ipv4 = IPV4};
                false ->
                    {error, {ip_no_match, IP}}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
configure({ipv6, IP}, State) ->
    case inet:parse_ipv6_address(IP) of
        {ok, IPV6} ->
            case is_match_local(IPV6) of
                true ->
                    State#state{ipv6 = IPV6};
                false ->
                    {error, {ip_no_match, IP}}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
configure({log_file, LogFile}, State) ->
    State#state{log_file = LogFile};
configure({session_max_time, {MaxTime}}, State) ->
    State#state{worker_max_time = MaxTime};
configure({pfmc, {IP, Port}}, State) when is_integer(Port)
                                        , 0 < Port
                                        , Port < 65535 ->
    case inet:parse_ipv4_address(IP) of
        {ok, _} ->
            State#state{pfmc_addr_port = {IP,Port}};
        {error, _Reason} ->
            {error, pfmc_ip}
    end;
configure({pfmc, {_, _Port}}, _State) ->
   {error, pfmc_port};
configure({nfs, _}, State) ->
    State;
configure(Config, State) ->
    error_logger:info_msg("config ~p unknow~n",[Config]),
    State.

is_match_local(IP) ->
    case inet:getifaddrs() of
        {ok, Nets} ->
            lists:foldl(fun(_, true) -> true;
                           ({_Name, IfOpts}, Result) ->
                                lists:foldl(fun(_Opt, true) -> true;
                                               (Opt, ROpt) ->
                                                   case Opt of
                                                       {addr, IP} -> true;
                                                       _ -> ROpt
                                                   end
                            end, Result, IfOpts)
                end, false, Nets);
        {error, Reason} ->
            info_manager:log_error("Config error", io_lib:format("~p",[Reason])),
            error_logger:info_msg("ips_emulator start error ~p~n", [Reason]),
            false
    end.
