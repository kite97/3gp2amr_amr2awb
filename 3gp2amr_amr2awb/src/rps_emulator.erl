%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2015 by wanglihe <wanglihe@ebupt.com>

-module(rps_emulator).

-behaviour(gen_server).

-include_lib("xmerl/include/xmerl.hrl").
-include("sdp.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(default_timeout, 1000).

-define(CONFIGFILE, "../etc/config").

-record(state, { log_file

               , ipv4 = none
               , ipv6 = none
               , pfmc_addr_port = none
               , worker_max
               , port_base
               , rcs
               , rps
               , initsdp
               , amr_trans
               , core_module_sup = none
	       , worker_max_time = infinity}).

-record(comm_node, { host
                   , port
                   , dfi
                   , msgr_dfi
                   , connected = false
                   , socket
                   , comm_server}).

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
            error_logger:info_msg("rps_emulator start error ~p~n", [Reason]),
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

        State when is_record(State, state) ->
            error_logger:tty(false),
            error_logger:logfile({open, State#state.log_file}),

            info_manager:add_handler(worker_event),
            info_manager:add_handler(conf_event),
            info_manager:add_handler(session_event),
            info_manager:add_handler(req_event),

            LocalIP = {State#state.ipv4, State#state.ipv6},
            WorkerMax = State#state.worker_max,
            PortBase = State#state.port_base,
            SSRCBase = State#state.port_base,
            AmrTrans = State#state.amr_trans,
            WorkerMaxTime = State#state.worker_max_time,
            %%如果不成功，emulator会退出，各种退出，以后告警
            NState = case core_module_sup:start_link( LocalIP
                                                    , WorkerMax
                                                    , PortBase
                                                    , SSRCBase
                                                    , AmrTrans
                                                    , WorkerMaxTime) of
                {ok, Pid} ->
                    info_manager:info("core_module_sup succ"),
                    State#state{core_module_sup = Pid};
                {error, ReasonCore} ->
                    info_manager:info("core_module_sup error ~p", [ReasonCore]),
                    exit(ReasonCore),
                    State
            end,

            case timer:send_interval(?default_timeout, try_connect) of
                {ok, _TRef} ->
                    case file:write_file("imp.pid", os:getpid()) of
                        ok ->
                            info_manager:log_info("Process start ok", "imp start ok"),
                            {ok, NState};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
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
handle_info(try_connect, #state{ rcs = Rcses
                               , rps = Rps
                               , initsdp = InitSdp} = State) ->
    NRcses = lists:map(fun(#comm_node{connected = true} = Rcs) ->
                                 Rcs;
                          (Rcs) ->
                                #comm_node{ host = RcsHost
                                          , port = RcsPortNum
                                          , msgr_dfi = RcsMsgrDfi
                                          , dfi = RcsDfi} = Rcs,
                                info_manager:info("connecting ~p", [{RcsHost, RcsDfi}]),
                                case gen_tcp:connect(RcsHost, RcsPortNum, [binary, {packet, 0}, {active, false}]) of
                                    {ok, Socket} ->
                                        info_manager:info("connect succ ~p", [{RcsHost, RcsDfi}]),
                                        #comm_node{ dfi = RpsDfi
                                                  , msgr_dfi = RpsMsgrDfi} = Rps,
                                        case comm_server:start_link( Socket
                                                                   , RcsMsgrDfi
                                                                   , RcsDfi
                                                                   , RpsMsgrDfi
                                                                   , RpsDfi
                                                                   , InitSdp) of
                                            {ok, Pid} ->
                                                info_manager:info("comm_server succ"),
                                                Rcs#comm_node{ connected = true
                                                             , socket = Socket
                                                             , comm_server = Pid};
                                            {error, Reason} ->
                                                info_manager:info("comm_server error ~p", [Reason]),
                                                catch gen_tcp:close(Socket),
                                                Rcs
                                        end;
                                    {error, Reason} ->
                                        info_manager:info("Rcs can not connect, error: ~p", [Reason]),
                                        Rcs
                                end
                       end, Rcses),
    {noreply, State#state{rcs = NRcses}};


handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#state.core_module_sup ->
    info_manager:info("core_module_sup exit with error: ~p", [Reason]),
    {stop, shutdown, State};

handle_info({'EXIT', Pid, Reason}, #state{rcs = Rcses} = State) ->
    info_manager:info("comm_server exit with error: ~p", [Reason]),
    NRcses = lists:map(fun(#comm_node{ comm_server = ServerPid
                                     , socket = Socket} = Rcs) when ServerPid =:= Pid->
                catch gen_tcp:close(Socket),
                Rcs#comm_node{ connected = false
                             , socket = none
                             , comm_server = none};
                          (Rcs) ->
                               Rcs
                       end, Rcses),
    {noreply, State#state{rcs = NRcses}};

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

configure(_, {error, _} = E) -> %%若配置有错，则后续不再解析
    E;
configure({worker_max, {Max}}, State) ->
    State#state{worker_max= Max};
configure({port_base, {Base}}, State) ->
    State#state{port_base = Base};

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

configure({comm, {RpsDomainId, ConfigComm}}, State) ->
    {Xml, _} = xmerl_scan:file("../etc/"++ConfigComm),
    {[MsgrFe, RcsFe, RpsFe], _} = lists:mapfoldl(fun(Name, ConfigXml) ->
                        XPath = "/configuration/programs/fe[@name='"++atom_to_list(Name)++"']",
                        [Fe] = xmerl_xpath:string(XPath, ConfigXml),
                        [#xmlAttribute{value = Id}] = xmerl_xpath:string("/fe/@id", Fe),
                        {Id, Xml}
                   end, Xml, [msgr, rcs, rps]),
    [RpsDomain|_] = xmerl_xpath:string("/configuration/processes/domain[@id='"++RpsDomainId++"']", Xml),
    {_, RpsDomainName, RpsHost, RpsMsgrInstance, RpsInstance} = get_node(RpsDomain),
    RpsMsgrDfi = trans({RpsDomainId, MsgrFe, RpsMsgrInstance}),
    RpsDfi = trans({RpsDomainId, RpsFe, RpsInstance}),
    Rps = #comm_node{ host = RpsHost
                    , dfi = RpsDfi
                    , msgr_dfi = RpsMsgrDfi},
    info_manager:info("get rps ~p", [Rps]),

    LinkSets = xmerl_xpath:string("/configuration/links/linkset[@domain1 != '"++RpsDomainName++"' and @domain2 = '"++RpsDomainName++"']", Xml),
    Rcses = lists:map(fun(LinkSet) ->
                              get_peer(Xml, LinkSet, RcsFe, MsgrFe)
                      end, LinkSets),
    info_manager:info("get rcses ~p", [Rcses]),
    State#state{ rcs = Rcses
               , rps = Rps };
configure({sdp, {ConfigSdp}}, State) ->
    case sdp:parse_sdp_config("../etc/"++ConfigSdp) of
        {ok, InitSdp} ->
            State#state{initsdp = InitSdp};
        {error, Reason} ->
            {error, Reason}
    end;
configure({pfmc, {IP, Port}}, State) when is_integer(Port)
                                        , 0 < Port
                                        , Port < 65535 ->
    case inet:parse_ipv4_address(IP) of
        {ok, _} ->
            State#state{pfmc_addr_port = {IP,Port}};
        {error, _Reason} ->
            {error, pfmc_ip}
    end;
configure({pfmc, {_, _}}, _) ->
   {error, pfmc_port};
configure({nfs, _}, State) ->
    State;
configure({amr_trans, {AmrTrans}}, State) ->
    State#state{amr_trans = AmrTrans};
configure(Config, State) ->
    error_logger:info_msg("config ~p unknow~n",[Config]),
    State.

get_peer(Xml, LinkSet, RcsFe, MsgrFe) ->
    [#xmlAttribute{value = RcsPortNumStr}] = xmerl_xpath:string("/linkset/@startport", LinkSet),
    RcsPortNum = list_to_integer(RcsPortNumStr),
    [#xmlAttribute{value = RcsDomainName}] = xmerl_xpath:string("/linkset/@domain1", LinkSet),
    [RcsDomain|_] = xmerl_xpath:string("/configuration/processes/domain[@name='"++RcsDomainName++"']", Xml),
    {RcsDomainId, _, RcsHost, RcsMsgrInstance, RcsInstance} = get_node(RcsDomain),
    MsgrDfi = trans({RcsDomainId, MsgrFe, RcsMsgrInstance}),
    RcsDfi = trans({RcsDomainId, RcsFe, RcsInstance}),
    #comm_node{ host = RcsHost
              , port = RcsPortNum
              , dfi = RcsDfi
              , msgr_dfi = MsgrDfi}.

get_node(XmlDomain) ->
    [#xmlAttribute{value = Id}] = xmerl_xpath:string("/domain/@id", XmlDomain),
    [#xmlAttribute{value = DomainName}] = xmerl_xpath:string("/domain/@name", XmlDomain),
    [#xmlAttribute{value = Host}] = xmerl_xpath:string("/domain/@host", XmlDomain),
    [MsgrLinkSet|_] = xmerl_xpath:string("/domain/subnet[@name='msgr']", XmlDomain),
    [#xmlAttribute{value = MsgrInstance}] = xmerl_xpath:string("/subnet/@startinstance", MsgrLinkSet),
    [XLinkSet|_] = xmerl_xpath:string("/domain/subnet[@name='rcs' or @name='rps' or @name='rms']", XmlDomain),
    [#xmlAttribute{value = XInstance}] = xmerl_xpath:string("/subnet/@startinstance", XLinkSet),
    {Id, DomainName, Host, MsgrInstance, XInstance}.

trans({A,B,C}) ->
    {list_to_integer(A),list_to_integer(B), list_to_integer(C)}.

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
