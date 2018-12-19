%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 8 March 2017 by wanglihe <wanglihe@ebupt.com>

-module(evs_decode).

-include("audio_core.hrl").

-define(EVS, "#!EVS_MC1.0\n").

-export([init/0, decode_frame/2, test/0]).

-on_load(load_lib/0).

load_lib() ->
    erlang:load_nif("../priv/evs_decode", 0).

init() ->
    "evs_decode init load fail".
decode_frame(_DecState, _Frame) ->
    "evs_decode init load fail".

test() ->
    {ok, Binary} = file:read_file("encoded.evs"),
    <<?EVS,1:32/big, Data/binary>> = Binary,
    EvsState = init(),
    D = loop(Data, 0, EvsState),
    file:write_file("decoded.pcm", list_to_binary(D)).

loop(Data, Pos, EvsState) when Pos < byte_size(Data) ->
    Len = audio_core:frame_bytes(Data, ?AUDIO_EVS),
    Bin = binary:part(Data, Pos, Len),
    {NEvsState, Pcm} = decode_frame(EvsState, Bin),
    io:format("~p~n", [byte_size(Pcm)]),
    [Pcm |loop(Data, Pos + Len, NEvsState)];
loop(_Data, _Pos, _EvsState) ->
    [].
