%%%-------------------------------------------------------------------
%%% File    : dtmf.erl
%%% Author  :  <wanglihe@ocarina.dev.ebupt.com>
%%% Description :
%%%
%%% Created : 31 May 2013 by  <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------

-module(dtmf).

-export([parse/2, init_dsp/0]).

-on_load(init/0).

init() ->
    erlang:load_nif("../priv/dtmf_parser", 0).

parse(_,_) ->
    "dtmf load fail".
init_dsp() ->
    "dtmf load fail".
