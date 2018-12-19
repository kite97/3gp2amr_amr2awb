%%%-------------------------------------------------------------------
%%% @author  :  <wanglihe@ocarina.dev.ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 26 Feb 2013 by wanglihe <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(icp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Sock, LocalIP, WorkerMax, PortBase, SSRCBase, WorkMaxTime) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ Sock
                                                     , LocalIP
                                                     , WorkerMax
                                                     , PortBase
                                                     , SSRCBase
						     , WorkMaxTime]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Sock, LocalIP, WorkerMax, PortBase, SSRCBase, WorkMaxTime]) ->
    WorkerPool = { worker_pool_daemon
                 , {module_daemon, start_link, [worker_pool, important, [WorkerMax, LocalIP, PortBase, SSRCBase, false, WorkMaxTime]]}
                 , permanent
                 , 2000
                 , worker
                 , [module_daemon]},
    ConfPool = { conf_pool_daemon
               , {module_daemon, start_link, [conf_pool, important, [WorkerMax]]}
               , permanent
               , 2000
               , worker
               , [module_daemon]},
    IcpServer = { icp_server_daemon
                , {module_daemon, start_link, [icp_server, important, [Sock]]}
                , permanent
                , 2000
                , worker
                , [module_daemon]},
    PulseServer = { pulse_server_daemon
                  , {module_daemon, start_link, [pulse_server, important, []]}
                  , permanent
                  , 2000
                  , worker
                  , [module_daemon]},
    _RingsCache = { rings_cache_daemon
                  , {module_daemon, start_link, [rings_cache, common, []]}
                  , permanent
                  , 2000
                  , worker
                  , [module_daemon]},

    %%所有的组件没有重启的必要，如果出错，就该会部退出，
    %%特别是rps断链这种，说明可以提供服务的源头没了，而
    %%且重启也没有意义，socket是连不上的。
    RestartStrategy = one_for_all,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [ WorkerPool
                    , ConfPool
                    , IcpServer
                    , PulseServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
