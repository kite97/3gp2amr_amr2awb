%%%-------------------------------------------------------------------
%%% File    : mixer.erl
%%% Author  : <lijinyu@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created : 6 Mar 2014 by <lijinyu@ocarina.dev.ebupt.com>
%%%-------------------------------------------------------------------
-module(mixer).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% API
-export([cast/2]).

-define(SERVER, ?MODULE).

-include("conference.hrl").
-include("audio_core.hrl").

-record(state, { conf
               , data_buffer
               , avail_databuf_channel_ids
               , conf_members = []
               , member_num = 0 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [self()], []).

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
init([Conf]) ->
    process_flag(trap_exit, true),
    {ok, #state{ conf = Conf
               , data_buffer = erlang:make_tuple(?MAXMIXCHANNEL, []) }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({join, Channel, DT}, _From, #state{ data_buffer = DataBuffer
                                              , member_num = N} = State) ->
    info_manager:info("mixer put ~p in channel ~p", [DT, Channel]),
    data_transfer:start_service(DT, {listen, {data, client, ?AUDIO_PCM8K16}}),
    data_transfer:start_service(DT, {listen, {digit, client}}),
    put(DT, Channel),
    put(Channel, DT),
    case N of
        0 ->
            pulse_server:add_player(self());
        _ ->
            ok
    end,
    {reply, {ok}, State#state{ data_buffer = setelement(Channel, DataBuffer, [])
                             , member_num = N + 1}};

handle_call({unjoin, Channel}, _From, #state{member_num = N} = State) ->
    info_manager:info("mixer ~p remove channel ~p", [self(), Channel]),
    case erase(Channel) of
        undefined ->
            {reply, {error, exist}, State};
        DT ->
            erase(DT),
            data_transfer:stop_service(DT, {listen, data}),
            case N of
                1 ->
                    pulse_server:remove_player(self());
                _ ->
                    ok
            end,
            {reply, ok, State#state{member_num = N - 1}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({data, DTPid, _Attribute, Data}, #state{data_buffer = DataBuffer} = State) ->
    case get(DTPid) of
        undefined ->
            {noreply, State};
        Channel ->
            Buf = element(Channel, DataBuffer),
            {noreply, State#state{data_buffer = setelement(Channel, DataBuffer, Buf ++ [Data])}}
    end;

handle_cast({digit, Digit}, State) ->
    Listeners = [L || {L,_} <- get() ,is_pid(L)],
    lists:map(fun(Listener) ->
                gen_server:cast(Listener, {none, mixer, Digit})
                end, Listeners),
    {noreply, State};

handle_cast(timer_pulse, #state{data_buffer = DataBuffer} = State) when State#state.member_num > 0 ->
    Members = [M || {M,_} <- get() ,is_integer(M)],
    DataToBeMixed = lists:map(fun(Member) ->
                                case element(Member, DataBuffer) of
                                    [Data|_] when byte_size(Data) =:= ?AUDIO_PCM8K16_FRAME_LEN ->
                                        Data;
                                    _ ->
                                        <<0:2560>>
                                end
                end, Members),
    DataMixed = tuple_to_list(audio_core:mix(list_to_tuple(DataToBeMixed), ?AUDIO_PCM8K16_FRAME_LEN)),
    lists:zipwith(fun(M, Data) ->
                        DTPid = get(M),
                        gen_server:cast(DTPid, {none, mixer, Data})
                  end, Members, DataMixed),
    NDataBuffer = list_to_tuple(lists:map(fun([]) -> [];
                                             ([_|T]) -> T
                                          end,
                                          tuple_to_list(DataBuffer))),
    {noreply, State#state{data_buffer = NDataBuffer}};

handle_cast(_Msg, State) ->
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
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Other API
%%--------------------------------------------------------------------
cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
