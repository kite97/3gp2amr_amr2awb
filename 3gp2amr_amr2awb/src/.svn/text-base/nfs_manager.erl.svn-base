-module(nfs_manager).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(LOCAL, "../voice/local/").

-record(state, { accessors = []
               , actives = []
               }).

start_link(Configs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Configs], []).

%%%-----------------------------------------------------%%%
%%%--------------------- init --------------------------%%%
%%%-----------------------------------------------------%%%
init([Configs]) ->
    case lists:keyfind(nfs, 1, Configs) of
        {nfs, []} ->
            {ok, #state{ accessors = []
                       , actives = []}};
        {nfs, RemoteDirs} when is_list(RemoteDirs) ->
            Accessors = lists:map(fun(Dir) ->
                                          {ok, Pid} = nfs_accessor:start_link(?LOCAL, Dir++"/"),
                                          info_manager:log_info("NFS", io_lib:format("nfs ~p up", [Dir])),
                                          {Pid, Dir, true}
                                  end
                                  , RemoteDirs),
            Actives = filter_actives(Accessors),
            timer:send_interval(5000, active_check),
            {ok, #state{ accessors = Accessors
                       , actives = Actives}};
        _ ->
            {ok, #state{ accessors = []
                       , actives = []}}
    end.

%%%-----------------------------------------------------%%%
%%%-------------------handle_call-----------------------%%%
%%%-----------------------------------------------------%%%
handle_call(get, _From, #state{actives = []} = State) ->
    {reply, {error, noactives}, State};
handle_call(get, _From, #state{actives = [Pid|_]} = State) ->
    {reply, {ok, Pid}, State};
handle_call(_Req, _From, State) ->
    {reply, {ok}, State}.

%%%-----------------------------------------------------%%%
%%%-------------------handle_cast-----------------------%%%
%%%-----------------------------------------------------%%%
handle_cast({nfs_up, Pid}, #state{accessors = Accessors} = State) ->
    info_manager:info("new active ~p in ~p",[Pid, Accessors]),
    NAccessors = update_accessors(Accessors, {Pid, nfs_up}),
    Actives = filter_actives(NAccessors),
    {noreply, State#state{ accessors = NAccessors
                         , actives = Actives}};
handle_cast({nfs_down, Pid}, #state{accessors = Accessors} = State) ->
    NAccessors = update_accessors(Accessors, {Pid, nfs_down}),
    Actives = filter_actives(NAccessors),
    case Actives of
        [] ->
            info_manager:log_error("NFS error", "ALL NFS UNAVAILABLE");
        _ ->
            ok
    end,
    {noreply, State#state{ accessors = NAccessors
                         , actives = Actives}};
handle_cast(_Req, State) ->
    {noreply, State}.

%%%-----------------------------------------------------%%%
%%%-------------------handle_info-----------------------%%%
%%%-----------------------------------------------------%%%
handle_info(active_check, #state{accessors = Accessors} = State) ->
    %%info_manager:info("new active check for ~p", [Accessors]),
    lists:foreach(fun({Pid, _, _}) ->
                    gen_server:cast(Pid, check)
                  end, Accessors),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------%%%
%%%-------------------inner_functions-------------------%%%
%%%-----------------------------------------------------%%%
update_accessors(Accessors, {Pid, Command}) ->
    lists:map(fun({APid, Dir, _}) when APid =:= Pid->
                      Active = case Command of
                                   nfs_up ->
                                       info_manager:log_info("NFS", io_lib:format("nfs ~p up", [Dir])),
                                       true;
                                   nfs_down ->
                                       info_manager:log_warn("NFS error"
                                                             , io_lib:format("nfs ~p down"
                                                                             , [Dir])),
                                       false
                               end,
                      {APid, Dir, Active};
                 (Accessor) ->
                      Accessor
              end, Accessors).
filter_actives(Accessors) ->
    lists:filtermap(fun(Accessor) ->
                            case Accessor of
                                {Pid, _, true} ->
                                    {true, Pid};
                                {_, _, false} ->
                                    false
                            end
                    end, Accessors).
