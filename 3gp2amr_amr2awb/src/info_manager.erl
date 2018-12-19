-module(info_manager).

-export([add_handler/1, add_handler/2, info/1, info/2
        , req/1, session/1, worker/1, conf/1, time/2, rtp/3]).
-export([log_fatal/2, log_error/2, log_warn/2, log_info/2, log_debug/2]).

-include("imp_log.hrl").

add_handler(Module, Args) ->
    case event_holder:start(Module, Args) of
        {ok, Pid} ->
            %%When parent exit without normal, it report message
            %%To avoid this, don't use start_link
            link(Pid),
            ok;
        Result ->
            Result
    end.

add_handler(Module) ->
    add_handler(Module, []).

req(Action) ->
    send_notify({req, Action}).

session(Action) ->
    send_notify({session, Action}).

worker(Action) ->
    send_notify({worker, Action}).

conf(Action) ->
    send_notify({conf, Action}).

time(Key, TimerStart) ->
    pfmc_daemon:cast({Key, TimerStart, os:timestamp()}).

rtp(Addr, Direct, Data) ->
    pfmc_daemon:cast({rtp, {Addr, Direct}, Data}).

info(M) ->
    io:format("~s: [info] ~s~n",[local_time(), M]).

info(F,M) ->
    Msg = lists:flatten(io_lib:format(F,M)),
    io:format("~s: [info] ~s~n",[local_time(), Msg]).

log_fatal(Key, Detail) ->
    send_notify({imp_log, {?FATAL, Key, Detail}}).
log_error(Key, Detail) ->
    send_notify({imp_log, {?ERROR, Key, Detail}}).
log_warn(Key, Detail) ->
    send_notify({imp_log, {?WARN, Key, Detail}}).
log_info(Key, Detail) ->
    send_notify({imp_log, {?INFO, Key, Detail}}).
log_debug(Key, Detail) ->
    send_notify({imp_log, {?DEBUG, Key, Detail}}).


send_notify(Notify) ->
    catch gen_event:notify(error_logger,Notify).

local_time() ->
    {{Year,Month,Day},{Hour,Min,Second}} = calendar:local_time(),
    io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",[Year,Month,Day,Hour,Min,Second]).
