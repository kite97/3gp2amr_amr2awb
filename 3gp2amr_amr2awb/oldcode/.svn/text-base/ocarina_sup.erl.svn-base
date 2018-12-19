
-module(ocarina_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CONFIGFILE, "../etc/config").

%% Helper macro for declaring children of supervisor
%% -define(CHILD(I, Type), {I, {interface, start_link}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Paras), {I, {module_daemon, start_link, Paras}, permanent, 5000, Type, [module_daemon]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case file:consult(?CONFIGFILE) of
        {ok, Configs} ->
            case info_manager:add_handler(log_event, ["../log/imp.log"]) of
                ok ->
                    info_manager:log_info("Process start", "imp starting"),
                    Emulator = ?CHILD(ips_emulator_daemon, worker, [ips_emulator, core, [Configs]]),
                    Status = ?CHILD(status_daemon, worker, [status, core, [Configs]]),
                    PfmcDaemon = ?CHILD(pfmc_daemon_daemon, worker, [pfmc_daemon, common, [Configs]]),
                    NfsManager = ?CHILD(nfs_manager_daemon, worker, [nfs_manager, common, [Configs]]),
                    {ok, { {one_for_one, 5, 900}, [Status, Emulator, PfmcDaemon, NfsManager]} };
                LogError ->
                    exit(LogError)
            end;
        {error, Reason} ->
            error_logger:info_msg("imp config error ~p~n", [Reason]),
            %%module init fail?? Core model error??
            info_manager:log_error("Config error", io_lib:format("~p", [Reason])),
            {stop, Reason}
    end.
