-module(rings_cache).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(RING_FILE, "rings.list").
-define(RING_FILE_PATH, "../voice/").
-define(RING_PATH, "../voice/local/").

-define(RELOAD_CIRCLE, 3600*3*1000).

-include("../include/wave.hrl").

-record(state, {rings_file}).

start_link() ->
    gen_server:start_link({local, ?SERVER},?MODULE, [?RING_FILE], []).

init([RingsFile]) ->
    ets:new(rings_list, [set, named_table, protected]),
    load_rings(RingsFile),
    timer:send_interval(?RELOAD_CIRCLE, reload),
    {ok, #state{rings_file = RingsFile}}.

handle_call(_Req, _From, State) ->
    {reply, {ok}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(reload, State) ->
    load_rings(State#state.rings_file),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load_rings(RingsFile) ->
    case file:open(?RING_FILE_PATH++RingsFile, read) of
        {ok, Rings} ->
	    load(Rings);
        {error, _Reason} ->
            info_manager:log_error("Model init", "open ring file error")
    end.

load(Rings) ->
    case file:read_line(Rings) of
        {ok, Data} ->
            Filename = string:strip(Data, both, $\n),
            try wave:read(?RING_PATH++Filename) of
                {Wave_Info, []} ->
                    ets:insert(rings_list, {Filename, Wave_Info});
                {Wave_Info, Error} ->
                    info_manager:log_debug("Nonstandard wave", io_lib:format("~p: ~p", [Filename, Error])),
                    ets:insert(rings_list, {Filename, Wave_Info})
            catch
                error:{badmatch, {error, enoent}} ->
                    info_manager:log_error("Model init", io_lib:format("~p ring not exist", [Filename]));
                _Class:_Reason ->
                    info_manager:log_error("Model init", io_lib:format("read ~p ring error",[Filename]))
            end,
            load(Rings);
        {error, _Reason} ->
            info_manager:log_error("Model init", "read ring file error");
        eof ->
            ok
    end.
