-module(ring_manager).

-export([update_file/1]).

-define(TIMEOUT, 3000).

%%%----------------------------------------------------------------%%%
%%%--------------------------- API --------------------------------%%%
%%%----------------------------------------------------------------%%%
update_file(File, Accessor) ->
    case gen_server:call(Accessor, {update_file, File}, ?TIMEOUT) of
        {ok, _Result} ->
            ok;
        {error, _Reason} ->
            error
    end.

update_file(File) ->
    case get_accessor() of
        {ok, Accessor} ->
            update_file(File, Accessor);
        {error, _Reason} ->
            error
    end.

get_accessor() ->
    gen_server:call(nfs_manager, get).


%%%----------------------------------------------------------------%%%
%%%------------------------inner_functions-------------------------%%%
%%%----------------------------------------------------------------%%%
