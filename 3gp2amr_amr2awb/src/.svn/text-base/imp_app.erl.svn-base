%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2015 by wanglihe <wanglihe@ebupt.com>

-module(imp_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    imp_sup:start_link().

stop(_State) ->
    ok.
