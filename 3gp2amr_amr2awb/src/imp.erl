%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2015 by wanglihe <wanglihe@ebupt.com>

-module(imp).

-export([main/1, start/0]).

main(_) ->
    %%maybe used as all things starter later
    ok.

start() ->
    application:start(imp).
