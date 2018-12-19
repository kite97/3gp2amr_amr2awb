%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2013, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2013 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------
-module(log_event).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-include("../include/imp_log.hrl").

-define(SERVER, ?MODULE).
-define(MAX_LINE, 5000).

-ifdef(log_level).
-define(LOG_LEVEL, case ?log_level of
                       fatal -> ?FATAL;
                       error -> ?ERROR;
                       warn -> ?WARN;
                       info -> ?INFO;
                       debug -> ?DEBUG;
                       _ -> ?INFO
                   end).
-else.
-define(LOG_LEVEL, ?INFO).
-endif.

-record(state, { file = none
               , filename
               , level = ?LOG_LEVEL
               , line_number = 0}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([FileName]) ->
    case file:open(FileName, [append]) of
        {ok, File} ->
            info_manager:info("log event open file ok"),
            {ok, #state{ file = File
                       , filename = FileName}};
        {error, Reason} ->
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(Msg, #state{ file = none , filename = FileName}) ->
    %%日志系统非系统相关，如果遇到磁盘满，或其他情况无法继续，停止即可
    case file:open(FileName, [append]) of
        {ok, File} ->
            handle_event(Msg, #state{ file = File
                                    , filename = FileName});
        {error, _Reason} ->
            {ok, #state{ filename = FileName}}
    end;
handle_event({imp_log, {Level, Key, Detail}}, #state{ file = File
                                                    , line_number = LineNumber} = State)
                                            when Level =< State#state.level
                                               , LineNumber < ?MAX_LINE ->
    case log_to_file(File, Level, Key, Detail) of
        ok ->
            {ok, State#state{line_number = LineNumber + 1}};
        _ ->
            {ok, State}
    end;
handle_event({imp_log, {Level, _, _}} = Msg, #state{ file = File
                                                   , filename = FileName} = State)
                                            when Level =< State#state.level ->
    {{Year,Month,Day},{Hour,Min,_}} = calendar:local_time(),
    BakName = "../log/imp.log." ++ io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B", [Year, Month, Day, Hour, Min]),
    file:close(File),
    file:rename(FileName, BakName),
    %%重新开始
    handle_event(Msg, #state{filename = FileName});
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{file = File}) ->
    file:close(File),
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
level_to_list(?FATAL) -> "FATAL";
level_to_list(?ERROR) -> "ERROR";
level_to_list(?WARN) -> "WARN";
level_to_list(?INFO) -> "INFO";
level_to_list(?DEBUG) -> "DEBUG".

log_to_file(File, Level, Key, Detail) ->
    {{Year,Month,Day},{Hour,Min,Second}} = calendar:local_time(),
    DateTime = io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",[Year,Month,Day,Hour,Min,Second]),
    catch io:format(File, "~s [~5s] [~s] ~s~n", [DateTime, level_to_list(Level), Key, Detail]).
