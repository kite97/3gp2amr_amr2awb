%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2014 by wanglihe <wanglihe@ebupt.com>

-module(audio_core).

-export([frame_bytes/2, evs_primary_bits/2]).
-export([convert/2, mix/2]).

-include("audio_core.hrl").

-on_load(init/0).

init() ->
    erlang:load_nif("../priv/audio_core", 0).


ulaw2pcm8k16(_Bin) ->
    "NIF library not loaded".
pcm8k162ulaw(_Bin) ->
    "NIF library not loaded".
alaw2pcm8k16(_Bin) ->
    "NIF library not loaded".
pcm8k162alaw(_Bin) ->
    "NIF library not loaded".
ulaw2alaw(_Bin) ->
    "NIF library not loaded".
alaw2ulaw(_Bin) ->
    "NIF library not loaded".
pcmu8tos16(_Bin) ->
    "NIF library not loaded".
pcm8kto16k(_Bin) ->
    "NIF library not loaded".
pcm16kto8k(_Bin) ->
    "NIF library not loaded".

mix_8khz(_Bin) ->
    "NIF library not loaded".
mix_16khz(_Bin) ->
    "NIF library not loaded".

frame_bytes(Data, MediaType) when MediaType =:= ?AUDIO_AMRNB
                                ; MediaType =:= ?AUDIO_AMRWB ->
    Mode = amr_codec:frame_mode(Data),
    amr_frame_bytes(MediaType, Mode);
frame_bytes(Data, MediaType) when MediaType =:= ?AUDIO_EVS ->
    <<_:2, _:2, Mode:4/big>> = binary_part(Data, 0, 1),
    case Mode of
        _ when 0 =< Mode , Mode =< 12 ->
            evs_primary_bits(MediaType, Mode) div 400 +1;
        _ ->
            1
    end;
frame_bytes(_, ?AUDIO_ULAW) -> ?AUDIO_ULAW_FRAME_LEN;
frame_bytes(_, ?AUDIO_ALAW) -> ?AUDIO_ALAW_FRAME_LEN;
frame_bytes(_, ?AUDIO_G729) -> ?AUDIO_G729_FRAME_LEN;
frame_bytes(_, ?AUDIO_PCM8K16) -> ?AUDIO_PCM8K16_FRAME_LEN.


amr_frame_bytes(?AUDIO_AMRNB, Mode) ->
    amr_codec:frame_bytes(amrnb, Mode);
amr_frame_bytes(?AUDIO_AMRWB, Mode) ->
    amr_codec:frame_bytes(amrwb, Mode).

evs_primary_bits(?AUDIO_EVS,0) -> 2800;
evs_primary_bits(?AUDIO_EVS,1) -> 7200;
evs_primary_bits(?AUDIO_EVS,2) -> 8000;
evs_primary_bits(?AUDIO_EVS,3) -> 9600;
evs_primary_bits(?AUDIO_EVS,4) -> 13200;
evs_primary_bits(?AUDIO_EVS,5) -> 16400;
evs_primary_bits(?AUDIO_EVS,6) -> 24400;
evs_primary_bits(?AUDIO_EVS,7) -> 32000;
evs_primary_bits(?AUDIO_EVS,8) -> 48000;
evs_primary_bits(?AUDIO_EVS,9) -> 64000;
evs_primary_bits(?AUDIO_EVS,10) -> 96000;
evs_primary_bits(?AUDIO_EVS,11) -> 128000;
evs_primary_bits(?AUDIO_EVS,12) -> 2400;
evs_primary_bits(?AUDIO_EVS,13) -> -1;
evs_primary_bits(?AUDIO_EVS,14) -> 0;
evs_primary_bits(?AUDIO_EVS,15) -> 0.

convert({<<>>, _}, _) ->
    error(empty_binary);
convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_AMRNB, Mode}) ->
    File_Mode = amr_codec:frame_mode(Bin),
    info_manager:info("amrnb: sdp mode is ~p, file mode is ~p~n", [Mode, File_Mode]),
    case {Mode, File_Mode} of
        {max, 7} ->
            Bin;
        {Mode1, Mode2} when Mode1 =:= Mode2->
            Bin;
        _ ->
            PCM = convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_PCM8K16, []}),
            convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode})
    end;
convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_AMRWB, Mode}) ->
    File_Mode = amr_codec:frame_mode(Bin),
    info_manager:info("amrwb: sdp mode is ~p, file mode is ~p~n", [Mode, File_Mode]),
    case {Mode, File_Mode} of
        {max, 8} ->
            Bin;
        {Mode1, Mode2} when Mode1 =:= Mode2->
            Bin;
        _ ->
            PCM = convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_PCM16K16, []}),
            convert({PCM, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode})
    end;
convert({Bin, ?AUDIO_EVS}, {?AUDIO_EVS, Mode}) ->
    <<_:4, FrameMode:4/big >> = binary_part(Bin, 0, 1),
    case {Mode, FrameMode} of
        {Mode1, Mode2} when Mode1 =:= Mode2->
            Bin;
        _ ->
            PCM = convert({Bin, ?AUDIO_EVS}, {?AUDIO_PCM8K16, []}),
            convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_EVS, Mode})
    end;
convert({Bin, SameType}, {SameType, []}) ->
    Bin;
%% 外部编解码器
convert({Bin, ?AUDIO_ULAW}, {?AUDIO_PCM8K16, _}) ->
    ulaw2pcm8k16(Bin);
convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_ULAW, _}) ->
    pcm8k162ulaw(Bin);
convert({Bin, ?AUDIO_ALAW}, {?AUDIO_PCM8K16, _}) ->
    alaw2pcm8k16(Bin);
convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_ALAW, _}) ->
    pcm8k162alaw(Bin);
convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_PCM8K16, _}) ->
    vox_codec:vox8k2pcm8k16(Bin);
convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_PCM8K16, _}) ->
    amr_codec:decode(amrnb, Bin);
convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, max}) ->
    amr_codec:encode(amrnb, Bin, 7);
convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode}) ->
    amr_codec:encode(amrnb, Bin, Mode);
convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_PCM16K16, _}) ->
    amr_codec:decode(amrwb, Bin);
convert({Bin, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, max}) ->
    amr_codec:encode(amrwb, Bin, 8);
convert({Bin, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode}) ->
    amr_codec:encode(amrwb, Bin, Mode);
convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_AMRWB, Mode}) ->
    PCM = convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []}),
    convert({PCM, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode});
convert({Bin, ?AUDIO_EVS}, {?AUDIO_PCM8K16, _}) ->
    evs_port:decode(Bin);
convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_EVS, Mode}) ->
    evs_port:encode(Bin, Mode);
%% 快速直转
convert({Bin, ?AUDIO_ULAW}, {?AUDIO_ALAW, _}) ->
    ulaw2alaw(Bin);
convert({Bin, ?AUDIO_ALAW}, {?AUDIO_ULAW, _}) ->
    alaw2ulaw(Bin);
%% 高级编码互转，仅支持部分互转，扩展功能时修改这里
%% 8K8BIT
convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_ULAW, _}) ->
    PCM = convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_ULAW, []});
convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_ALAW, _}) ->
    PCM = convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_ALAW, []});
convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_EVS, Mode}) ->
    PCM = convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_EVS, Mode});
convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_AMRNB, Mode}) ->
    PCM = convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode});
convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_AMRWB, Mode}) ->
    PCM = convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_PCM16K16, []}),
    convert({PCM, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode});
%% ulaw
convert({Bin, ?AUDIO_ULAW}, {?AUDIO_AMRNB, Mode}) ->
    PCM = convert({Bin, ?AUDIO_ULAW}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode});
convert({Bin, ?AUDIO_ULAW}, {?AUDIO_AMRWB, Mode}) ->
    PCM8K16 = convert({Bin, ?AUDIO_ULAW}, {?AUDIO_PCM8K16, []}),
    PCM16K16 = convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []}),
    convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode});
convert({Bin, ?AUDIO_ULAW}, {?AUDIO_EVS, Mode}) ->
    PCM = convert({Bin, ?AUDIO_ULAW}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_EVS, Mode});
%% alaw
convert({Bin, ?AUDIO_ALAW}, {?AUDIO_AMRNB, Mode}) ->
    PCM = convert({Bin, ?AUDIO_ALAW}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode});
convert({Bin, ?AUDIO_ALAW}, {?AUDIO_AMRWB, Mode}) ->
    PCM8K16 = convert({Bin, ?AUDIO_ALAW}, {?AUDIO_PCM8K16, []}),
    PCM16K16 = convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []}),
    convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode});
convert({Bin, ?AUDIO_ALAW}, {?AUDIO_EVS, Mode}) ->
    PCM = convert({Bin, ?AUDIO_ALAW}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_EVS, Mode});
%% vox
convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_ALAW, _}) ->
    PCM = convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_ALAW, []});
convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_ULAW, _}) ->
    PCM = convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_ULAW, []});
convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_EVS, Mode}) ->
    PCM = convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_EVS, Mode});
convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_AMRNB, Mode}) ->
    PCM = convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode});
convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_AMRWB, Mode}) ->
    PCM8K16 = convert({Bin, ?AUDIO_VOX8K}, {?AUDIO_PCM8K16, []}),
    PCM16K16 = convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []}),
    convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode});
%% evs
convert({Bin, ?AUDIO_EVS}, {?AUDIO_ALAW, _}) ->
    PCM = convert({Bin, ?AUDIO_EVS}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_ALAW, []});
convert({Bin, ?AUDIO_EVS}, {?AUDIO_ULAW, _}) ->
    PCM = convert({Bin, ?AUDIO_EVS}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_ULAW, []});
convert({Bin, ?AUDIO_EVS}, {?AUDIO_AMRNB, Mode}) ->
    PCM = convert({Bin, ?AUDIO_EVS}, {?AUDIO_PCM8K16, []}),
    convert({PCM, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode});
convert({Bin, ?AUDIO_EVS}, {?AUDIO_AMRWB, Mode}) ->
    PCM8K16 = convert({Bin, ?AUDIO_EVS}, {?AUDIO_PCM8K16, []}),
    PCM16K16 = convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []}),
    convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode});
%% amrnb
convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_ALAW, []}) ->
    PCM8K16 = convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_PCM8K16, []}),
    convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_ALAW, []});
convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_ULAW, []}) ->
    PCM8K16 = convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_PCM8K16, []}),
    convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_ULAW, []});
convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_AMRWB, Mode}) ->
    PCM8K16 = convert({Bin, ?AUDIO_AMRNB}, {?AUDIO_PCM8K16, []}),
    PCM16K16 = convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, []}),
    convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_AMRWB, Mode});
%%amrwb
convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_ALAW, []}) ->
    PCM16K16 = convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_PCM16K16, []}),
    PCM8K16 = convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_PCM8K16, []}),
    convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_ALAW, []});
convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_ULAW, []}) ->
    PCM16K16 = convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_PCM16K16, []}),
    PCM8K16 = convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_PCM8K16, []}),
    convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_ULAW, []});
convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_AMRNB, Mode}) ->
    PCM16K16 = convert({Bin, ?AUDIO_AMRWB}, {?AUDIO_PCM16K16, []}),
    PCM8K16 = convert({PCM16K16, ?AUDIO_PCM16K16}, {?AUDIO_PCM8K16, []}),
    convert({PCM8K16, ?AUDIO_PCM8K16}, {?AUDIO_AMRNB, Mode});
%% PCM核心态互转
%% freq, only 16bit
convert({Bin, ?AUDIO_PCM16K16}, {?AUDIO_PCM8K16, _}) ->
    pcm_convert(?AUDIO_16K16BIT, ?AUDIO_8K16BIT, Bin);
convert({Bin, ?AUDIO_PCM8K16}, {?AUDIO_PCM16K16, _}) ->
    pcm_convert(?AUDIO_8K16BIT, ?AUDIO_16K16BIT, Bin);
%% bit, only 8 -> 16
convert({Bin, ?AUDIO_PCM16K8}, {?AUDIO_PCM16K16, _}) ->  %% 不实现16bit转8bit
    pcm_convert(?AUDIO_16K8BIT, ?AUDIO_16K16BIT, Bin);
convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_PCM8K16, _}) ->    %% 不实现16bit转8bit
    pcm_convert(?AUDIO_8K8BIT, ?AUDIO_8K16BIT, Bin);

convert({Bin, ?AUDIO_PCM8K8}, {?AUDIO_PCM16K16, _}) ->
    pcm_convert(?AUDIO_8K8BIT, ?AUDIO_16K16BIT, Bin);
convert({Bin, FormatFrom}, {FormatTo, _}) ->
    info_manager:log_error("Format convert unsupport", io_lib:format("~p to ~p", [FormatFrom, FormatTo])),
    Bin.

pcm_convert({Rate, ?AUDIO_8BIT}, {Rate, ?AUDIO_16BIT}, Bin) ->
    pcmu8tos16(Bin);
%%暂时无需求
%%pcm_convert({Rate, 16}, {Rate, 8}, Bin) when ->
%%  Bin;
pcm_convert(?AUDIO_8K16BIT, ?AUDIO_16K16BIT, Bin) ->
    pcm8kto16k(Bin);
pcm_convert(?AUDIO_16K16BIT, ?AUDIO_8K16BIT, Bin) ->
    pcm16kto8k(Bin);
pcm_convert({SrcRate, SrcBit}, {DestRate, DestBit}, Bin) ->
    PCM = pcm_convert({SrcRate, SrcBit}, {SrcRate, DestBit}, Bin),
    pcm_convert({SrcRate, DestBit}, {DestRate, DestBit}, PCM).

%%混音接口
mix(Bin, ?AUDIO_PCM8K16_FRAME_LEN) ->
    mix_8khz(Bin);
mix(Bin, ?AUDIO_PCM16K16_FRAME_LEN) ->
    mix_16khz(Bin).
