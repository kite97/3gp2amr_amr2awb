%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 8 March 2017 by wanglihe <wanglihe@ebupt.com>

-module(evs_port).

-include("audio_core.hrl").

-define(EVS, "#!EVS_MC1.0\n").

-export([new_enocder/1, new_decoder/0, delete/1, test/0]).
-export([encode/2, decode/1, encode_frame/2, decode_frame/2]).
new_enocder(Bitereate) ->
    Command = "../priv/evs_encoder_port",
    Port = open_port({spawn_executable, Command}, [ use_stdio
                                           , stream
                                           , binary
                                           , {args,[integer_to_list(Bitereate)]}]),
    drop_head(),
    Port.

drop_head() ->
    receive
        {_, {data, _H}} ->
            ok
    end.

new_decoder() ->
    Command = "../priv/evs_decoder_port",
    Port = open_port({spawn_executable, Command}, [ use_stdio
                                           , stream
                                           , binary]),
    put_head(Port),
    Port.

put_head(Port) ->
    port_command(Port, <<?EVS, 1:32/big>>).

decode(Bin) ->
    Decoder = new_decoder(),
    list_to_binary(decode(Decoder, Bin, 0)).

decode(Decoder, Bin, Pos) when Pos < byte_size(Bin) ->
    FrameLen = audio_core:frame_bytes(Bin, ?AUDIO_EVS),
    Frame = binary:part(Bin, Pos, FrameLen),
    port_command(Decoder, Frame),
    Pcm = receive
        {_,{data, E}} ->
            E
    end,
    [Pcm |decode(Decoder, Bin, Pos+FrameLen)];
decode(Decoder,_,_) ->
    catch port_close(Decoder),
    [].

encode(Bin, Mode) ->
    BitRate = audio_core:evs_primary_bits(?AUDIO_EVS, Mode),
    Encoder = new_enocder(BitRate),
    list_to_binary(encode(Encoder, Bin, 0)).

encode(Encoder, Bin, Pos) when Pos < byte_size(Bin) ->
    PcmLen = audio_core:frame_bytes(Bin, ?AUDIO_PCM8K16),
    Pcm =  case Pos + PcmLen of
                 End when End < byte_size(Bin) ->
                     binary:part(Bin, Pos, PcmLen);
                 _ ->
                     F0 = binary:part(Bin, Pos, byte_size(Bin) - Pos),
                     PadLen = Pos+PcmLen-byte_size(Bin),
                     Pad = binary:copy(<<0>>, PadLen),
                     <<F0/binary, Pad/binary>>
             end,
    port_command(Encoder, Pcm),
    Frame = receive
        {_,{data, E}} ->
            E
    end,
    [Frame |encode(Encoder, Bin, Pos+PcmLen)];
encode(Encoder,_,_) ->
    catch port_close(Encoder),
    [].

encode_frame(Encoder, Pcm) ->
    port_command(Encoder, Pcm),
    receive
        {_,{data, E}} ->
            E
    end.
decode_frame(Decoder, Frame) ->
    port_command(Decoder, Frame),
    receive
        {_,{data, D}} ->
            D
    end.

test() ->
    EncoderPort = new_enocder(8000),
    DecoderPort = new_decoder(),
    {ok, Bin} = file:read_file("input.pcm"),
    %%<<?EVS, 1:32/big, Pcm/binary>> = Bin,
    loop(EncoderPort, DecoderPort, Bin, 0).

loop(EncoderPort, DecoderPort, Pcm, Pos) when Pos < byte_size(Pcm)->
    Frame =  case Pos + 320 of
                 End when End < byte_size(Pcm) ->
                     binary:part(Pcm, Pos, 320);
                 _ ->
                     F0 = binary:part(Pcm, Pos, byte_size(Pcm) - Pos),
                     PadLen = Pos+320-byte_size(Pcm),
                     Pad = binary:copy(<<0>>, PadLen),
                     <<F0/binary, Pad/binary>>
             end,
    io:format("Frame size ~p~n", [byte_size(Frame)]),
    port_command(EncoderPort, Frame),
    Edata = receive
        {_,{data, E}} ->
            %%ok
            io:format("E ~p~n", [byte_size(E)]),
            E
    end,
    port_command(DecoderPort, Edata),
    _Ddata = receive
        {_,{data, D}} ->
            %%ok
            io:format("D ~p~n", [byte_size(D)]),
            D
    end,
    loop(EncoderPort, DecoderPort, Pcm, Pos + 320);
loop(EncoderPort, DecoderPort, _Pcm, _Pos) ->
    delete(EncoderPort),
    delete(DecoderPort).

delete(Port) ->
    port_close(Port).
