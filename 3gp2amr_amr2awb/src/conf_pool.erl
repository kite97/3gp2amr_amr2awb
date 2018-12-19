%%%-------------------------------------------------------------------
%%% File    : conf_pool.erl
%%% Author  : <lijinyu@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created : 10 Mar 2014 by <lijinyu@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(conf_pool).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([cast/1]).

-define(SERVER, ?MODULE).

-define(CONFIDBASE, 0).

-record(state, { room_remain = 0
               , next_id = 0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ConfMax) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfMax], []).

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
init([ConfMax]) ->
    process_flag(trap_exit, true),
    ets:new(?SERVER, [set, named_table, protected]),
    info_manager:conf({reset, ConfMax}),
    info_manager:info("new conference pool ~n"),
    {ok, #state{room_remain = ConfMax}}.

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
handle_cast({Commander, {alloc_conf, {_, RoomID}, _, MetaData}}, #state{room_remain = N} = State) when N =< 0 ->
    gen_server:cast(Commander, {alloc_conf, RoomID, {error, econfmax}, MetaData}),
    {noreply, State};
handle_cast({Commander, {alloc_conf, {CallID, new}, Command, MetaData}} = _Alloc, State) ->
    RoomID = State#state.next_id,
    NAlloc = {Commander, {alloc_conf, {CallID, RoomID}, Command, MetaData}},
    handle_cast(NAlloc, State#state{next_id = (RoomID +1) band 16#FF});
handle_cast({Commander, {alloc_conf, {CallID, RoomID}, _, MetaData}} = Alloc, #state{room_remain = N} = State) ->
    case conf:start_link(RoomID) of
        {ok, Pid} ->
            put(Pid, RoomID),
            put(RoomID, Pid),
            put(CallID, RoomID),
            info_manager:info("create conference ~p:~p~n", [RoomID, Pid]),
            conf:cast(Pid, Alloc),
            info_manager:conf(start),
            {noreply, State#state{room_remain = N - 1}};
        _ ->
            %%再议错误类型
            gen_server:cast(Commander, {alloc_conf, RoomID, {error, econfmax}, MetaData}),
            info_manager:session(session_fail),
            {noreply, State}
    end;
handle_cast({Commander, {release_conf, {CallID, RoomID}, _, MetaData}}, #state{room_remain = N} = State) ->
    case erase(RoomID) of
        undefined ->
            gen_server:cast(Commander, {release_conf, RoomID, {error, exist}, MetaData}),
            {noreply, State};
        Pid ->
            conf:cast(Pid, release),
            erase(Pid),
            erase(CallID),
            info_manager:conf(stop),
            gen_server:cast(Commander, {release_conf, RoomID, {ok}, MetaData}),
            {noreply, State#state{room_remain = N + 1}}
    end;

handle_cast({Commander, {Type, {callid, CallID}, Para, MetaData}}, State) when Type =:= stop_confpa_media
                                                                             ; Type =:= stop_conf_dc
                                                                             ; Type =:= stop_confpr_media ->
    case get(CallID) of
        undefined ->
            gen_server:cast(Commander, {Commander, CallID, {error, exist}, MetaData}),
            {noreply, State};
        RoomID ->
            gen_server:cast(self(), {Commander, {Type, RoomID, Para, MetaData}}),
            {noreply, State}
    end;

handle_cast({Commander, {ControlTag , RoomID, _, MetaData}} = Control, State) when ControlTag =:= join_conf
                                                                                 ; ControlTag =:= unjoin_conf
                                                                                 ; ControlTag =:= conf_play
                                                                                 ; ControlTag =:= conf_digit_collect
                                                                                 ; ControlTag =:= conf_record_media
                                                                                 ; ControlTag =:= stop_confpa_media
                                                                                 ; ControlTag =:= stop_conf_dc
                                                                                 ; ControlTag =:= stop_confpr_media ->
    case get(RoomID) of
        undefined ->
            gen_server:cast(Commander, {ControlTag, RoomID, {error, exist}, MetaData}),
            {noreply, State};
        Pid ->
            conf:cast(Pid, Control),
            {noreply, State}
    end;

handle_cast(Msg, State) ->
    info_manager:info("conf_pool receive unknown msg ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    info_manager:conf({reset, 0}).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------
cast(Msg) ->
    catch gen_server:cast(?SERVER, {self(), Msg}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
