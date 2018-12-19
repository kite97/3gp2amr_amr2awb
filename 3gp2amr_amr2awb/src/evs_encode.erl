%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 8 March 2017 by wanglihe <wanglihe@ebupt.com>

-module(evs_encode).

-include("audio_core.hrl").

-define(EVS, "#!EVS_MC1.0\n").

-export([init/1, encode_frame/2, test/0]).

-on_load(load_lib/0).

load_lib() ->
    erlang:load_nif("../priv/evs_encode", 0).

init(_Mode) ->
    "evs_decode init load fail".
encode_frame(_DecState, _Frame) ->
    "evs_decode init load fail".

test() ->
    {ok, Binary} = file:read_file("input.pcm"),
    EvsState = init(8000),
    {ok, F} = file:open("./encoded.evs", [write, binary]),
    D = [<<?EVS,1:32/big>>|loop(Binary, 0, EvsState)],
    file:write(F, list_to_binary(D)).

loop(Data, Pos, EvsState) when Pos < byte_size(Data) ->
    Len = 160*2,
    Bin = case Pos of
              _ when Pos + Len < byte_size(Data) ->
                    binary:part(Data, Pos, Len);
              _ ->
                    binary:part(Data, Pos, byte_size(Data) - Pos)
          end,
    {NEvsState, Frame} = encode_frame(EvsState, Bin),
    [Frame|loop(Data, Pos + Len, NEvsState)];
loop(_Data, _Pos, _EvsState) ->
    [].
