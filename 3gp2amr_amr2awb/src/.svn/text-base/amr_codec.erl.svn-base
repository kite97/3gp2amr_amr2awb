%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 14 May 2014 by wanglihe <wanglihe@ebupt.com>

-module(amr_codec).

-export([frame_bits/2, frame_bytes/2, frame_mode/1]).
-export([init/2, encode/3, decode/2, encode_frame/4, decode_frame/3]).
-export([change_bitrate/3, change_frame_bitrate/2]).

-on_load(load_lib/0).

load_lib() ->
    erlang:load_nif("../priv/amr_codec", 0).

init(amrnb, encode) ->
    amrnb_encode_init();
init(amrnb, decode) ->
    amrnb_decode_init();
init(amrwb, encode) ->
    amrwb_encode_init();
init(amrwb, decode) ->
    amrwb_decode_init().

encode(amrnb, PCMs, Mode) ->
    amrnb_encode(PCMs, Mode);
encode(amrwb, PCMs, Mode) ->
    amrwb_encode(PCMs, Mode).

encode_frame(amrnb, EState, PCM, Mode) ->
    amrnb_encode_frame(EState, PCM, Mode);
encode_frame(amrwb, EState, PCM, Mode) ->
    amrwb_encode_frame(EState, PCM, Mode).

decode(amrnb, Frames) ->
    amrnb_decode(Frames);
decode(amrwb, Frames) ->
    amrwb_decode(Frames).

decode_frame(amrnb, DState, Frame) ->
    amrnb_decode_frame(DState, Frame);
decode_frame(amrwb, DState, Frame) ->
    amrwb_decode_frame(DState, Frame).

change_frame_bitrate(Frame, {amrnb, EState, DState, Mode}) ->
    {NDState, PCM} = amrnb_decode_frame(DState, Frame),
    {NEState, NFrame} = amrnb_encode_frame(EState, PCM, Mode),
    {NFrame, {amrnb, NEState, NDState, Mode}};
change_frame_bitrate(Frame, {amrwb, EState, DState, Mode}) ->
    {NDState, PCM} = amrwb_decode_frame(DState, Frame),
    {NEState, NFrame} = amrwb_encode_frame(EState, PCM, Mode),
    {NFrame, {amrwb, NEState, NDState, Mode}}.

change_bitrate(Codec, Frames, Mode) ->
    EState = init(Codec, encode),
    DState = init(Codec, decode),
    FMode = frame_mode(Frames),
    FrameBytes = frame_bytes(Codec, FMode),
    {NFrames, _} = lists:mapfoldl(fun change_frame_bitrate/2
            , {Codec, EState, DState, Mode}
            , [X || <<X:FrameBytes/binary>> <= Frames]),
    list_to_binary(NFrames).



amrnb_encode_init() ->
    "amrnb_encode_init load fail".
amrnb_decode_init() ->
    "amrnb_decode_init load fail".

amrnb_encode_frame(_State, _Bin, _Mode) ->
    "amrnb_convert load fail".
amrnb_decode_frame(_State, _Bin) ->
    "amrnb_convert load fail".

amrnb_encode(Bin, Mode) ->
    State = amrnb_encode_init(),
    {F, _} = lists:mapfoldl(fun(PCM, EState) ->
                {NEState, Frame} = amrnb_encode_frame(EState, PCM, Mode),
                {Frame, NEState}
        end, State, [X || <<X:320/binary>> <= Bin]),
    list_to_binary(F).

amrnb_decode(Bin) ->
    State = amrnb_decode_init(),
    list_to_binary(amrnb_decode(State, Bin, 0)).
amrnb_decode(State, Data, Pos) ->
    Mode = frame_mode(Data),
    FrameBytes = amrnb_frame_bytes(Mode),
    case  Pos + FrameBytes of
        NPos when NPos < byte_size(Data) ->
            Frame = binary_part(Data, Pos, FrameBytes),
            {NState, PCM} = amrnb_decode_frame(State, Frame),
            [PCM |amrnb_decode(NState, Data, NPos)];
        NPos when NPos =:= byte_size(Data) ->
            Frame = binary_part(Data, Pos, FrameBytes),
            {_NState, PCM} = amrnb_decode_frame(State, Frame),
            [PCM];
        _ ->
            []
    end.

amrwb_encode_init() ->
    "amrwb_encode_init load fail".
amrwb_decode_init() ->
    "amrwb_decode_init load fail".

amrwb_encode_frame(_State, _Bin, _Mode) ->
    "amrwb_convert load fail".
amrwb_decode_frame(_State, _Bin) ->
    "amrwb_convert load fail".

amrwb_encode(Bin, Mode) ->
    State = amrwb_encode_init(),
    {F, _} = lists:mapfoldl(fun(PCM, EState) ->
                {NEState, Frame} = amrwb_encode_frame(EState, PCM, Mode),
                {Frame, NEState}
        end, State, [X || <<X:640/binary>> <= Bin]),
    list_to_binary(F).
amrwb_decode(Bin) ->
    State = amrwb_decode_init(),
    list_to_binary(amrwb_decode(State, Bin, 0)).
amrwb_decode(State, Data, Pos) ->
    Mode = frame_mode(Data),
    FrameBytes = amrwb_frame_bytes(Mode),
    case  Pos + FrameBytes of
        NPos when NPos < byte_size(Data) ->
            Frame = binary_part(Data, Pos, FrameBytes),
            {NState, PCM} = amrwb_decode_frame(State, Frame),
            [PCM |amrwb_decode(NState, Data, NPos)];
        NPos when NPos =:= byte_size(Data) ->
            Frame = binary_part(Data, Pos, FrameBytes),
            {_NState, PCM} = amrwb_decode_frame(State, Frame),
            [PCM];
        _ ->
            []
    end.

frame_bits(amrnb, Mode) when 0 =< Mode , Mode =< 11 ->
   amrnb_frame_bits(Mode);
frame_bits(amrnb, Mode) when Mode =:= 15 ->
   amrnb_frame_bits(Mode);
frame_bits(amrwb, Mode) when 0 =< Mode , Mode =< 9 ->
   amrwb_frame_bits(Mode);
frame_bits(amrwb, Mode) when Mode =:= 14
                           ; Mode =:= 15 ->
   amrwb_frame_bits(Mode).

%% 根据 3gpp ts_126101v110000p.pdf
amrnb_frame_bits(0) ->  95;
amrnb_frame_bits(1) -> 103;
amrnb_frame_bits(2) -> 118;
amrnb_frame_bits(3) -> 134;
amrnb_frame_bits(4) -> 148;
amrnb_frame_bits(5) -> 159;
amrnb_frame_bits(6) -> 204;
amrnb_frame_bits(7) -> 244;
amrnb_frame_bits(8) -> 39;
amrnb_frame_bits(9) -> 43;
amrnb_frame_bits(10) -> 38;
amrnb_frame_bits(11) -> 37;
%%amrnb_frame_bits(12) -> 0;
%%amrnb_frame_bits(13) -> 0;
%%amrnb_frame_bits(14) -> 0;
amrnb_frame_bits(15) -> 0.

%% 根据 3gpp ts_126201v110000p.pdf
amrwb_frame_bits(0) -> 132;
amrwb_frame_bits(1) -> 177;
amrwb_frame_bits(2) -> 253;
amrwb_frame_bits(3) -> 285;
amrwb_frame_bits(4) -> 317;
amrwb_frame_bits(5) -> 365;
amrwb_frame_bits(6) -> 397;
amrwb_frame_bits(7) -> 461;
amrwb_frame_bits(8) -> 477;
amrwb_frame_bits(9) -> 40;
%%amrwb_frame_bits(10) -> 0;
%%amrwb_frame_bits(11) -> 0;
%%amrwb_frame_bits(12) -> 0;
%%amrwb_frame_bits(13) -> 0;
amrwb_frame_bits(14) -> 0;
amrwb_frame_bits(15) -> 0.

%%bits与bytes的换算方法如下
%%frame_bytes = bits_to_bytes(bits) + 1
%%
%%bits_to_bytes(Bits) ->
%%    case Bits rem 8 of
%%        0 -> Bits div 8;
%%        _ -> (Bits div 8) + 1
%%    end.

%% frame_bytes只用于文件外传，文件只支持非SID的mode
frame_bytes(amrnb, Mode) when 0 =< Mode , Mode =< 7 ->
    amrnb_frame_bytes(Mode);
frame_bytes(amrwb, Mode) when 0 =< Mode , Mode =< 8 ->
    amrwb_frame_bytes(Mode).

amrwb_frame_bytes(0) -> 18;
amrwb_frame_bytes(1) -> 24;
amrwb_frame_bytes(2) -> 33;
amrwb_frame_bytes(3) -> 37;
amrwb_frame_bytes(4) -> 41;
amrwb_frame_bytes(5) -> 47;
amrwb_frame_bytes(6) -> 51;
amrwb_frame_bytes(7) -> 59;
amrwb_frame_bytes(8) -> 61.

amrnb_frame_bytes(0) -> 13;
amrnb_frame_bytes(1) -> 14;
amrnb_frame_bytes(2) -> 16;
amrnb_frame_bytes(3) -> 18;
amrnb_frame_bytes(4) -> 20;
amrnb_frame_bytes(5) -> 21;
amrnb_frame_bytes(6) -> 27;
amrnb_frame_bytes(7) -> 32.

frame_mode(Frames) ->
    <<_:1, FT:4/unsigned, _Q:1/unsigned, _:2>> = binary_part(Frames, 0, 1),
    FT.
