%%%-------------------------------------------------------------------
%%% File    : pulse_server.erl
%%% Author  :  <wanglihe@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created : 26 Apr 2013 by  <wanglihe@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(pulse_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add_player/1, remove_player/1]).

-define(SERVER, ?MODULE).
-define(default_timeout, 20).
-record(state, {players}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    %%process_flag(priority, high),
    case timer:send_interval(?default_timeout,timer_pulse) of
        {ok, _TRef} ->
	    info_manager:info("~p start~n", [?MODULE]),
            {ok, #state{players = ets:new(?SERVER, [set, protected])}};
        {error, Reason} ->
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, Player}, State) ->
    info_manager:info("~p add player ~p~n", [?MODULE, Player]),
    Players = State#state.players,
    catch link(Player),
    ets:insert(Players, {Player}),
    {noreply, State};
handle_cast({remove, Player}, State) ->
    info_manager:info("~p remove player ~p~n", [?MODULE, Player]),
    Players = State#state.players,
    unlink(Player),
    ets:delete(Players, Player),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timer_pulse, State) ->
    Players = State#state.players,
    %%info_manager:info("~p get timer_pulse ~p~n", [?MODULE, os:timestamp()]),
    spawn(fun() ->
            ets:foldl(fun({Player}, _) ->
                gen_server:cast(Player, timer_pulse)
            end, ok, Players)
          end),
    {noreply, State};
handle_info({'EXIT', Player, _Reason}, State) ->
    Players = State#state.players,
    ets:delete(Players, Player),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    Players = State#state.players,
    ets:foldl(fun({Player}, _) ->
            unlink(Player) end, true, Players),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_player(Player) ->
    gen_server:cast(?SERVER, {add, Player}).
remove_player(Player) ->
    gen_server:cast(?SERVER, {remove, Player}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

