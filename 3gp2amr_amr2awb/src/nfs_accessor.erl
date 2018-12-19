-module(nfs_accessor).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 3000).

-record(state, { port = none
               , port_pid = none
               , active = true
               , local_path
               , remote_path}).

start_link(Local, Remote) ->
    gen_server:start_link(?MODULE, [Local, Remote], []).

%%%-----------------------------------------------------%%%
%%%--------------------- init --------------------------%%%
%%%-----------------------------------------------------%%%
init([Local, Remote]) ->
    process_flag(trap_exit, true),
    info_manager:info("accessor get remote ~p", [Remote]),
    {ok, #state{ local_path = Local
               , remote_path = Remote}}.

%%%-----------------------------------------------------%%%
%%%-------------------handle_call-----------------------%%%
%%%-----------------------------------------------------%%%
handle_call({update_file, _File}, _From, #state{active = false} = State) ->
    {reply, {error, timeout}, State};
handle_call({update_file, File}, _From, #state{ port = Port
                                              , active = true
                                              , port_pid = Pid} = State) ->
    case accessor_call(Port, File) of
        {ok, Result} ->
            {reply, {ok, Result}, State};
        {error, Result} when Result =/= timeout ->
            {reply, {error, Result}, State};
        {error, timeout} ->
            gen_server:cast(nfs_manager, {nfs_down, self()}),
            catch port_close(Port),
            os:cmd("kill "++integer_to_list(Pid)),
            {reply, {error, timeout}, State#state{ port = none
                                                 , port_pid = none
                                                 , active = false}}
    end;
handle_call(_Req, _From, State) ->
    {reply, {ok}, State}.

%%%-----------------------------------------------------%%%
%%%-------------------handle_cast-----------------------%%%
%%%-----------------------------------------------------%%%
handle_cast(check, #state{ port = none
                         , local_path = Local
                         , remote_path = Remote} = State) ->
    Port = open_port( {spawn_executable, "../priv/accessor"}
                    , [ stream
                      , binary
                      , use_stdio
                      , {args, [ Local
                               , Remote]}]),
    {os_pid, Pid} = erlang:port_info(Port, os_pid),
    info_manager:info("new port ~p", [Port]),
    handle_cast(check, State#state{ port = Port, port_pid = Pid});
handle_cast(check, #state{ port = Port
                         , active = Active
                         , port_pid = Pid} = State) ->
    case accessor_call(Port, "active_test") of
        {ok, _Result} when Active ->
            {noreply, State};
        {ok, _Result} ->
            gen_server:cast(nfs_manager, {nfs_up, self()}),
            {noreply, State#state{active = true}};
        {error, Result} ->
            info_manager:info("nfs down for ~p", [Result]),
            case Active of
                true ->
                    gen_server:cast(nfs_manager, {nfs_down, self()});
                false ->
                    ok
            end,
            case Result of
                timeout ->
                    catch port_close(Port),
                    os:cmd("kill " ++ integer_to_list(Pid)),
                    {noreply,  State#state{ port = none
                                          , port_pid = none
                                          , active = false}};
                _ ->
                    {noreply, State#state{ active = false}}
            end
    end;
handle_cast(_Req, State) ->
    {noreply, State}.

%%%-----------------------------------------------------%%%
%%%-------------------handle_info-----------------------%%%
%%%-----------------------------------------------------%%%
handle_info({'EXIT', Port, _}, #state{port = Port} = State) ->
    gen_server:cast(nfs_manager, {nfs_down, self()}),
    {noreply, State#state{ port = none
                         , port_pid = none
                         , active = false}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------%%%
%%%-------------------inner_functions-------------------%%%
%%%-----------------------------------------------------%%%
%%似乎没有可能触发，但就是出现了相关crash错误日志，不稳定重现
%%加入相关代码，后续观察
%%现在天津现网明确的出现了此种错误。原因是带呼叫启动，在nfs未
%%初始化完成时，就收到update file请求，造成此错误。
accessor_call(none, _File) ->
    {error, code_trans(99)};
accessor_call(Port, File) ->
    erlang:port_command(Port, File++"\n"),
    receive
        {Port, {data, <<1:8/unsigned>>}} ->
            {ok, code_trans(1)};
        {Port, {data, <<2:8/unsigned>>}} ->
            {ok, code_trans(2)};
        {Port, {data, <<Code:8/unsigned>>}} ->
            {error, code_trans(Code)}
    after
        ?TIMEOUT ->
            {error, timeout}
    end.

code_trans(1)   -> no_update;
code_trans(2)   -> updated;
code_trans(99) -> err_port_none;
code_trans(100) -> err_max_length;
code_trans(101) -> err_remote;
code_trans(102) -> err_local;
code_trans(103) -> err_read;
code_trans(104) -> err_write;
code_trans(105) -> err_rename.
