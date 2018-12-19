%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2015 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(core_module_sup).

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
start_link(LocalIP, WorkerMax, PortBase, SSRCBase, AmrTrans, WorkMaxTime) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ LocalIP
                                                     , WorkerMax
                                                     , PortBase
                                                     , SSRCBase
                                                     , AmrTrans
						     , WorkMaxTime ]).

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
init([LocalIP, WorkerMax, PortBase, SSRCBase, AmrTrans, WorkMaxTime]) ->
    WorkerPool = { worker_pool_daemon
                 , {module_daemon, start_link, [worker_pool, important, [WorkerMax, LocalIP, PortBase, SSRCBase, AmrTrans, WorkMaxTime]]}
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

    RestartStrategy = one_for_one,
    MaxRestarts = 4,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [ PulseServer
                    , WorkerPool
                    , ConfPool]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
