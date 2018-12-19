%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 28 Nov 2014 by wanglihe <wanglihe@ebupt.com>

-module(vox_codec).

-export([vox8k2pcm8k16/1]).

-on_load(load_lib/0).

load_lib() ->
    erlang:load_nif("../priv/vox_codec", 0).

vox8k2pcm8k16(_Bin) ->
    "vox8k2pcm8k16 load fail".
