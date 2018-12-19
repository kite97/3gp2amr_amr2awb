%%%-------------------------------------------------------------------
%%% @author lijinyu <lijinyu@ebupt.com>
%%% @copyright (C) 2015, lijinyu
%%% @doc
%%%
%%% @end
%%% Created : 6 March 2015 by lijinyu <lijinyu@ebupt.com>
%%%-------------------------------------------------------------------

-module(sdp).

-export([parse_sdp_config/1, parse_remote_sdp/1, generate/1, negotiate/2]).

-include("sdp.hrl").

-define(CONFIGFILE, "../etc/config.sdp").

-define(Unparse, ["i", "u", "e", "p", "b", "z", "k", "t", "r"]).

-define(EvsBitrate, ["5.9", "7.2", "8", "9.6", "13.2", "16.4", "24.4", "32", "48", "64", "96", "128"]).

parse_sdp_config(SdpConfig) ->
    case file:consult(SdpConfig) of
        {ok, Configs} ->
            case lists:foldr(fun(Config, Sdp) ->
                                    configure(Config, Sdp)
                                end
                                , #sdp{}
                                , Configs) of
                InitSdp when is_record(InitSdp, sdp) ->
                    #sdp{audio = #media_descript{codeclist = AuCodecList}
                       , video = #media_descript{codeclist = ViCodecList}} = InitSdp,
                    {ok, InitSdp#sdp{audio = (InitSdp#sdp.audio)#media_descript{codeclist = AuCodecList}
                                    ,video = (InitSdp#sdp.video)#media_descript{codeclist = ViCodecList}}};
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.

configure(_, {error, _} = E) -> %%若配置有错，则后续不再解析
    E;
configure({ulaw}, #sdp{audio = #media_descript{codeclist = CodecList}} = Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = [#codec{type = ulaw, payload = 0}|CodecList]}};
configure({alaw}, #sdp{audio = #media_descript{codeclist = CodecList}} = Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = [#codec{type = alaw, payload = 8}|CodecList]}};
configure({g723}, #sdp{audio = #media_descript{codeclist = CodecList}} = Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = [#codec{type = g723, payload = 4}|CodecList]}};
configure({g729}, #sdp{audio = #media_descript{codeclist = CodecList}} = Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = [#codec{type = g729, payload = 18}|CodecList]}};
configure({telephone, {pt, PT}, {rate, Rate}}, #sdp{audio = #media_descript{digit = Digit}} = Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{digit = [#codec{type = telephone, payload = PT
                                                                  ,ext = #ext{rate = Rate}}|Digit]}};
configure({evs, {pt, PT}, {rate, Rate}}, #sdp{audio = #media_descript{codeclist = CodecList}} = Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = [#codec{ type = evs, payload = PT
                                                                      , ext = #ext{rate = Rate}}|CodecList]}};
configure({Codecname, {pt, PT}, {rate, Rate}, {align, Align}, {modeset, Modeset}}
        , #sdp{audio = #media_descript{codeclist = CodecList}} = Sdp) when Codecname =:= amrnb; Codecname =:= amrwb ->
    case catch lists:all(fun(I) -> check_para(I) end, [{align, Align}, {modeset, {Codecname, Modeset}}]) of
        true ->
            Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = [#codec{ type = Codecname, payload = PT
                                                    , ext = #ext{rate = Rate, align = Align, modeset = Modeset}}|CodecList]}};
        false ->
            {error, "amr para value configuration error"};
        {'EXIT', _} ->
            {error, "amr para format configuration error"}
    end;
configure({h263}, #sdp{video = #media_descript{codeclist = CodecList}} = Sdp) ->
    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{codeclist = [#codec{type = h263, payload = 34}|CodecList]}};
configure({h264, {pt, PT}, {rate, Rate}}, #sdp{video = #media_descript{codeclist = CodecList}} = Sdp) ->
    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{codeclist = [#codec{type = h264, payload = PT
                                                                     , ext = #ext{rate = Rate}}|CodecList]}};
configure({addrtype, {Addrtype, {default, Value}}} = Para, Sdp) ->
    case catch check_para(Para) of
        true ->
            NAddrtype = lists:map(fun(I) -> convert_addrtype(I) end, Addrtype),
            Sdp#sdp{addrtype = {NAddrtype, {default, convert_addrtype(Value)}}};
        false ->
            {error, "addrtype value configuration error"};
        {'EXIT', _} ->
            {error, "addrtype format configuration error"}
    end;
configure({negotype, {Negotype}}, Sdp) when Negotype =:= common; Negotype =:= remote ->
    Sdp#sdp{negotype = {Negotype}};
configure({negotype, {local, {{audio, Au}, {video, Vi}}} = Negotype}, Sdp) when erlang:is_list(Au),
                                                                                erlang:is_list(Vi) ->
    Sdp#sdp{negotype = Negotype};
configure({negotype, _}, _) ->
    {error, "negotype configuration error"};
configure(_Config, State) ->
    %io:format("config ~p unknow~n", [Config]),
    State.

check_para({align, {Value, {default, Default}}}) ->
    case lists:all(fun(I) -> lists:member(I, [oct, eff]) end, Value) of
        true ->
            lists:member(Default, Value);
        false ->
            false
    end;
check_para({modeset, {Codecname, {Modeset, {default, Value}}}}) ->
    lists:all(fun(I) -> check_modeset(I) end, [{Codecname, Modeset}, {Value, Modeset}]);
check_para({addrtype, {Value, {default, Default}}}) ->
    case lists:all(fun(I) -> lists:member(I, [ipv4, ipv6]) end, Value) of
        true ->
            lists:member(Default, Value);
        false ->
            false
    end.

check_modeset({amrnb, Modeset}) ->
    lists:all(fun(I) -> lists:member(I, [0,1,2,3,4,5,6,7]) end, Modeset);
check_modeset({amrwb, Modeset}) ->
    lists:all(fun(I) -> lists:member(I, [0,1,2,3,4,5,6,7,8]) end, Modeset);
check_modeset({Default, Modeset}) ->
    lists:all(fun(I) -> lists:member(I, Modeset) end, Default).

convert_addrtype(ipv4) ->
    inet;
convert_addrtype(ipv6) ->
    inet6.

generate(#sdp{addr = Addr, addrtype = Addrtype, sendrecv_mark = IsExist, sendrecv = Sendrecv} = Sdp) ->
    case catch generate_media_descripts(Sdp) of
        {'EXIT', _R} ->
            {error, "generate error: input sdp record error"};
        {error, E} ->
            {error, E};
        MediaDescript ->
            {ok, string:concat(string:join(lists:filter(fun(I) -> I =/= none end,
                                      [ ?V_LINE
                                      , generate_o_line(Addrtype, inet:ntoa(Addr)) %%由于未解析remoteSdp中的o行，所以解析的输出
                                      , ?S_LINE                                    %%作为generate的输入时，生成的字符串与最初的
                                      , generate_c_line(Addrtype, inet:ntoa(Addr)) %%解析的输入不一致，o行的地址类型和地址会随
                                      , ?T_LINE                                    %%c行的而改变
                                      , generate_sendrecv(IsExist, Sendrecv)
                                      , MediaDescript
                                      , generate_non_media_descripts(Sdp)]), "\r\n"), "\r\n")}
    end;
generate(_) ->
    {error, "generate sdp error: input para error"}.

generate_non_media_descripts(#sdp{app = [], text = []}) ->
    none;
generate_non_media_descripts(#sdp{app = Apps, text = []}) ->
    string:join(lists:map(fun(App) -> generate_non_media_m_line(?APP, App) end, Apps), "\r\n");
generate_non_media_descripts(#sdp{app = [], text = Texts}) ->
    string:join(lists:map(fun(Text) -> generate_non_media_m_line(?TEXT, Text) end, Texts), "\r\n");
generate_non_media_descripts(#sdp{app = Apps, text = Texts}) ->
    string:join(lists:flatten([lists:map(fun(App) -> generate_non_media_m_line(?APP, App) end, Apps)
                              ,lists:map(fun(Text) -> generate_non_media_m_line(?TEXT, Text) end, Texts)]), "\r\n").

generate_non_media_m_line(Type, #media_descript{proto = Proto, codeclist = Payloads}) ->
    string:join([ string:concat(?M_LINE, Type)
                , integer_to_list(0)
                , Proto
                , string:join(Payloads, " ")], " ").

generate_media_descripts(#sdp{audio = #media_descript{codeclist = AuCodecList, digit = Digit}
                            , video = #media_descript{codeclist = ViCodecList}} = Sdp) ->
    case {lists:append(AuCodecList, Digit), ViCodecList} of
        {[], []} ->
            {error, "generate media descripts error: all codeclists are empty"};
        {NAuCodecList, []} ->
            generate_media_descript(audio, NAuCodecList, Sdp#sdp.audio);
        {[], _} ->
            generate_media_descript(video, ViCodecList, Sdp#sdp.video);
        {NAuCodecList, _} ->
            string:join([generate_media_descript(audio, NAuCodecList, Sdp#sdp.audio)
                        ,generate_media_descript(video, ViCodecList, Sdp#sdp.video)], "\r\n")
    end.

generate_media_descript(Mediatype, Codeclist, #media_descript{port = 0}) ->
    generate_m_line(Mediatype, 0, generate_m_payloads(Codeclist));
generate_media_descript(Mediatype, Codeclist, #media_descript{port = Port} = Mediadescript) ->
    case generate_media_aline(Mediadescript) of
        [] ->
            generate_media_mline(Mediatype, Port, Codeclist);
        Alines ->
            string:join([generate_media_mline(Mediatype, Port, Codeclist), Alines], "\r\n")
    end.

generate_m_payloads(Codeclist) ->
    lists:foldr(fun(I, Payloads) -> [integer_to_list(I#codec.payload)|Payloads] end, [], Codeclist).

generate_media_aline(#media_descript{sendrecv = Sendrecv, ptime = PTime, maxptime = Maxptime}) ->
    string:join(lists:filter(fun(I) -> I =/= none end,
                              lists:map(fun(A) -> generate_a_line(A) end,
                                         [{Sendrecv}, {ptime, PTime}, {maxptime, Maxptime}])), "\r\n").

generate_media_mline(Mediatype, Port, Codeclist) ->
    {Payloads, Aline} = lists:foldl(fun(Codec, I) -> codec_to_str(Codec, I) end, {[],[]}, Codeclist),
    Mline = generate_m_line(Mediatype, Port, lists:reverse(Payloads)),
    case Aline of
        [] ->
            Mline;
        _ ->
            string:join([Mline, string:join(lists:reverse(Aline), "\r\n")], "\r\n")
    end.

codec_to_str({codec, Codecname, Payload, #ext{rate = Rate, channel = Ch, others = Others
                                            , align = Align,     align_mark = Alignmark
                                            , modeset = Modeset, modeset_mark = Modemark
                                            , mode_switch = Modeswitch, bitrate = Bitrate
                                            , framerate = Framerate, framerate_mark = Ratemark
                                            , framesize = Framesize, framesize_mark = Sizemark}}, {Payloads, Aline}) ->
    NPayloads = [integer_to_list(Payload)|Payloads],
    NAline = case
                 case {Rate, Ch} of
                     {none, none} ->
                         none;
                     {_, none} when is_integer(Rate) ->
                         integer_to_list(Rate);
                     {_, _} when is_integer(Rate)
                               , is_integer(Ch) ->
                         string:join([integer_to_list(Rate), integer_to_list(Ch)], "/");
                     _ ->
                         none
                 end
             of
                 none ->
                     Aline;
                 NRate ->
                     [generate_a_line({rtpmap, Payload, string:join([trans_codecname(Codecname), NRate], "/")})|Aline]
             end,
    case Codecname of
        _ when Codecname =:= ulaw; Codecname =:= alaw;
               Codecname =:= g723; Codecname =:= g729 ->
            {NPayloads, NAline};
        telephone ->
            case Others of
                [] ->
                    {NPayloads, NAline};
                [Value] ->
                    TeleFmtp = generate_a_line({fmtp, Payload, Value}),
                    {NPayloads, [TeleFmtp|NAline]}
            end;
        _ when Codecname =:= amrnb; Codecname =:= amrwb ->
            Paras = lists:map(fun(Para) -> generate_amr_para(Para) end, [{others, Others}
                                                                       , {align, Align, Alignmark}
                                                                       , {modeset, Modeset, Modemark, Codecname}]),
            case lists:filter(fun(I) -> I =/= none end, Paras) of
                [] ->
                    {NPayloads, NAline};
                FinalParas ->
                    {NPayloads, [generate_a_line({fmtp, Payload, string:join(FinalParas, ";")})|NAline]}
            end;
        evs ->
            Paras = lists:map(fun(Para) -> generate_evs_para(Para) end, [{others, Others}
                                                                       , {bitrate, Bitrate}
                                                                       , {mode_switch, Modeswitch}]),
            case lists:filter(fun(I) -> I =/= none end, Paras) of
                [] ->
                    {NPayloads, NAline};
                FinalParas ->
                    {NPayloads, [generate_a_line({fmtp, Payload, string:join(FinalParas, ";")})|NAline]}
            end;
        _ when Codecname =:= h263; Codecname =:= h264 ->
            Paras = lists:map(fun(Para) -> generate_video_para(Para) end, [{others, Others, Payload}
                                                                         , {framerate, Framerate, Ratemark}
                                                                         , {framesize, Framesize, Payload, Sizemark}]),
            case lists:filter(fun(I) -> I =/= none end, Paras) of
                [] ->
                    {NPayloads, NAline};
                FinalParas ->
                    {NPayloads, [string:join(FinalParas, "\r\n")|NAline]}
            end;
        _ ->
            {Payloads, Aline}
    end.

trans_codecname(alaw) -> ?PCMA;
trans_codecname(ulaw) -> ?PCMU;
trans_codecname(g723) -> ?G723;
trans_codecname(g729) -> ?G729;
trans_codecname(amrnb) -> ?AMR;
trans_codecname(amrwb) -> ?AMRWB;
trans_codecname(evs) -> ?EVS;
trans_codecname(telephone) -> ?TELEPHONE;
trans_codecname(h263) -> ?H263;
trans_codecname(h264) -> ?H264.

generate_a_line({none}) ->
    none;
generate_a_line({Attr}) ->
    string:concat(?A_LINE, atom_to_list(Attr));
generate_a_line({_, none}) ->
    none;
generate_a_line({Attr, Value}) when is_atom(Value) ->
    string:join([string:concat(?A_LINE, atom_to_list(Attr)), atom_to_list(Value)], ":");
generate_a_line({Attr, Value}) when is_integer(Value) ->
    string:join([string:concat(?A_LINE, atom_to_list(Attr)), integer_to_list(Value)], ":");
generate_a_line({Attr, Value}) when is_float(Value) ->
    string:join([string:concat(?A_LINE, atom_to_list(Attr)), float_to_list(Value)], ":");
generate_a_line({Attr, Payload, Value}) ->
    string:join([string:join([string:concat(?A_LINE, atom_to_list(Attr))
                            , integer_to_list(Payload)], ":"), Value], " ").

generate_m_line(Media, Port, Fmt) ->
    string:join([ string:concat(?M_LINE, atom_to_list(Media))
                , integer_to_list(Port)
                , ?PROTO
                , string:join(Fmt, " ")], " ").

generate_o_line(Addrtype, Addr) ->
    generate_line(?O_LINE, Addrtype, Addr).

generate_c_line(Addrtype, Addr) ->
    generate_line(?C_LINE, Addrtype, Addr).

generate_line(Start, inet, Addr) ->
    string:join([Start, ?IP4, Addr], " ");
generate_line(Start, inet6, Addr) ->
    string:join([Start, ?IP6, Addr], " ");
generate_line(Start, {_, {default, inet}}, Addr) ->
    string:join([Start, ?IP4, Addr], " ");
generate_line(Start, {_, {default, inet6}}, Addr) ->
    string:join([Start, ?IP6, Addr], " ").

generate_sendrecv(false, _) ->
    none;
generate_sendrecv(true, Sendrecv) ->
    generate_a_line({Sendrecv}).

generate_amr_para({others, []}) ->
    none;
generate_amr_para({others, Others}) ->
    string:join(Others, ";");
generate_amr_para({align, {_, {default, oct}}, false}) ->
    "octet-align=1";
generate_amr_para({align, {_, {default, eff}}, false}) ->
    "octet-align=0";
generate_amr_para({align, _, false}) ->
    none;
generate_amr_para({align, Align, true}) ->
    case Align of
        oct ->
            "octet-align=1";
        eff ->
            "octet-align=0"
    end;
generate_amr_para({modeset, [0,1,2,3,4,5,6,7], false, amrnb}) ->
    none;
generate_amr_para({modeset, [0,1,2,3,4,5,6,7,8], false, amrwb}) ->
    none;
generate_amr_para({modeset, Modeset, _, _}) ->
    case Modeset of
        {_, {default, Value}} ->
            string:concat("mode-set=", string:join(lists:map(fun(I) -> integer_to_list(I) end, Value), ","));
        _ ->
            string:concat("mode-set=", string:join(lists:map(fun(I) -> integer_to_list(I) end, Modeset), ","))
    end.

generate_evs_para({others, []}) ->
    none;
generate_evs_para({others, Others}) ->
    string:join(Others, ";");
generate_evs_para({bitrate, BR}) ->
    string:concat("br=" , BR);
generate_evs_para({mode_switch, MS}) ->
    string:concat("evs-mode-switch=" , integer_to_list(MS)).

generate_video_para({framerate, _, false}) ->
    none;
generate_video_para({framerate, Framerate, true}) ->
    generate_a_line({framerate, Framerate});
generate_video_para({framesize, _, _, false}) ->
    none;
generate_video_para({framesize, {Width, Height}, Payload, true}) ->
    Framesize = string:join([integer_to_list(Width), integer_to_list(Height)], "-"),
    generate_a_line({framesize, Payload, Framesize});
generate_video_para({others, [], _}) ->
    none;
generate_video_para({others, Others, Payload}) ->
    generate_a_line({fmtp, Payload, string:join(Others, ";")}).

negotiate(none, InitSdp) when is_record(InitSdp, sdp) ->
    {ok, InitSdp};
negotiate(#sdp{addrtype = RAddrtype, addr = RAddr, sendrecv_mark = IsExist, sendrecv = Sendrecv
                 , audio = #media_descript{port = RPortA, codeclist = RAuCodeclist, digit = RDigit, sendrecv = AuSendrecv}
                 , video = #media_descript{port = RPortV, codeclist = RViCodeclist, sendrecv = ViSendrecv}} = RSdp
            , #sdp{negotype = Negotype, addrtype = {IAddrtype, _}
                 , audio = #media_descript{codeclist = IAuCodeclist, digit = IDigit}
                 , video = #media_descript{codeclist = IViCodeclist}}) ->
    case lists:member(RAddrtype, IAddrtype) of
        true ->
            {_, Audio} = negotiate_media(RAuCodeclist, IAuCodeclist),
            {_, Video} = negotiate_media(RViCodeclist, IViCodeclist),
            {_, Digit} = negotiate_media(RDigit, IDigit),
            Sdp = #sdp{addrtype = RAddrtype, addr = RAddr
                     , sendrecv_mark = IsExist, sendrecv = negotiate_sendrecv(Sendrecv)
                     , app = RSdp#sdp.app, text = RSdp#sdp.text
                     , audio = RSdp#sdp.audio, video = RSdp#sdp.video},
            case {Audio, Video, Digit} of
                {[], [], _} ->  %%协商失败
                    {error, "negotiation fails"};
                {[], _, _} when RPortV =:= 0 ->  %%协商失败
                    {error, "negotiation fails"};
                {_, [], _} when RPortA =:= 0 ->  %%协商失败
                    {error, "negotiation fails"};
                {[], ViCodeclist, _} when RAuCodeclist =/= [], RPortA =/= 0
                                        , IAuCodeclist =:= [] -> %%initSdp中未配置音频，音频m行端口置0
                    Negosdp = Sdp#sdp{audio = (RSdp#sdp.audio)#media_descript{port = 0}
                                     , video = (RSdp#sdp.video)#media_descript{codeclist = ViCodeclist
                                                                             , sendrecv = negotiate_sendrecv(ViSendrecv)}},
                    deal_negotype(Negotype, Negosdp);
                {[], _ViCodeclist, _} when RAuCodeclist =/= [], RPortA =/= 0
                                         , IAuCodeclist =/= [] -> %%音视频中音频协商失败返回协商失败，兼容rps
                    {error, "negotiation fails: audio resource failed"};
                {AuCodeclist, [], NDigit} when RViCodeclist =/= [], RPortV =/= 0
                                             , IViCodeclist =:= [] -> %%initSdp中未配置视频，视频m行端口置0
                    Negosdp = Sdp#sdp{audio = (RSdp#sdp.audio)#media_descript{codeclist = AuCodeclist, digit = NDigit
                                                                             , sendrecv = negotiate_sendrecv(AuSendrecv)}
                                     , video = (RSdp#sdp.video)#media_descript{port = 0}},
                    deal_negotype(Negotype, Negosdp);
                {_AuCodeclist, [], _} when RViCodeclist =/= [], RPortV =/= 0
                                         , IViCodeclist =/= [] -> %%音视频中视频协商失败返回协商失败，兼容rps
                    {error, "negotiation fails: video resource failed"};
                {AuCodeclist, ViCodeclist, NDigit} ->
                    Negosdp = case {RPortA, RPortV} of %%ptime和maxptime没有协商，协商结果取remote中的值
                        {_, 0} ->
                            Sdp#sdp{audio = (RSdp#sdp.audio)#media_descript{codeclist = AuCodeclist, digit = NDigit
                                                                          , sendrecv = negotiate_sendrecv(AuSendrecv)}};
                        {0, _} ->
                            Sdp#sdp{video = (RSdp#sdp.video)#media_descript{codeclist = ViCodeclist
                                                                          , sendrecv = negotiate_sendrecv(ViSendrecv)}};
                        {_, _} ->
                            Sdp#sdp{audio = (RSdp#sdp.audio)#media_descript{codeclist = AuCodeclist, digit = NDigit
                                                                          , sendrecv = negotiate_sendrecv(AuSendrecv)}
                                  , video = (RSdp#sdp.video)#media_descript{codeclist = ViCodeclist
                                                                          , sendrecv = negotiate_sendrecv(ViSendrecv)}}
                    end,
                    deal_negotype(Negotype, Negosdp)
            end;
        false ->
            {error, "negotiation fails: addrtype failed"}
    end;
negotiate(_, _) ->
    {error, "negotiate error: input para error"}.

negotiate_media(RCodeclist, ICodeclist) ->
    lists:foldr(fun(Codec, {Codeclist, Result}) ->
                       negotiate_codec(Codec, {Codeclist, Result}) end, {ICodeclist, []}, RCodeclist).

negotiate_codec(#codec{type = Codecname, ext = RExt} = RCodec, {ICodeclist, Result}) ->
    case lists:keyfind(Codecname, 2, ICodeclist) of
        false ->
            {ICodeclist, Result};
        {codec, _, _, _} when Codecname =:= ulaw; Codecname =:= alaw;
                              Codecname =:= g723; Codecname =:= g729 ->
            case {RExt#ext.rate, RExt#ext.channel} of
                {none, none} ->
                    {ICodeclist, [RCodec|Result]};
                {8000, Ch} when Ch =:= 1; Ch =:= none ->
                    {ICodeclist, [RCodec|Result]};
                _ ->
                    {ICodeclist, Result}
            end;
        {codec, telephone, _, _Ext} -> %%假定只支持采样率为8000和16000的telephone-event,待验证
            case RExt#ext.rate of      %%没有协商Ext#ext.others中的内容
                Rate when Rate =:= 8000; Rate =:= 16000 ->
                    {ICodeclist, [RCodec|Result]};
                _ ->
                    {ICodeclist, Result}
            end;
        {codec, _, _, IExt} when Codecname =:= amrnb; Codecname =:= amrwb ->
            AmrResult = lists:map(fun(I) -> negotiate_amr_ext(I) end, [{rate, RExt#ext.rate, Codecname}
                                                                      ,{channel, RExt#ext.channel}
                                                                      ,{align, RExt#ext.align, IExt#ext.align}
                                                                      ,{modeset, RExt#ext.modeset, IExt#ext.modeset}]),
            case lists:all(fun(I) -> lists:nth(1, tuple_to_list(I)) end, AmrResult) of
                true ->
                    [{true, Modeset}] = lists:filter(fun(I) -> erlang:tuple_size(I) =:= 2 end, AmrResult),
                    {ICodeclist, [RCodec#codec{ext = RExt#ext{modeset = Modeset}}|Result]};
                false ->
                    {ICodeclist, Result}
            end;
        {codec, _, _, _IExt} when Codecname =:= evs ->
            EvsResult = lists:map(fun(I) -> negotiate_evs_ext(I) end, [{rate, RExt#ext.rate}
                                                                      ,{channel, RExt#ext.channel}
                                                                      ,{mode_switch, RExt#ext.mode_switch}
                                                                      ,{bitrate, RExt#ext.bitrate}]),
            case lists:all(fun(I) -> I end, EvsResult) of
                true ->
                    {ICodeclist, [RCodec|Result]};
                false ->
                    {ICodeclist, Result}
            end;
        {codec, _, _, _IExt} when Codecname =:= h263; Codecname =:= h264 -> %%framerate,framesize未协商
            VideoResult = lists:map(fun(I) -> negotiate_video_ext(I) end, [{rate, RExt#ext.rate}]),
            case lists:all(fun(I) -> lists:nth(1, tuple_to_list(I)) end, VideoResult) of
                true ->
                    {ICodeclist, [RCodec|Result]};
                false ->
                    {ICodeclist, Result}
            end;
        _ ->
            {ICodeclist, [RCodec|Result]}
    end.

negotiate_amr_ext({rate, 8000, amrnb}) ->
    {true};
negotiate_amr_ext({rate, _, amrnb}) ->
    {false};
negotiate_amr_ext({rate, 16000, amrwb}) ->
    {true};
negotiate_amr_ext({rate, _, amrwb}) ->
    {false};
negotiate_amr_ext({channel, Ch}) when Ch =:= 1; Ch =:= none ->
    {true};
negotiate_amr_ext({channel, _}) ->
    {false};
negotiate_amr_ext({align, RAlign, {IAlign, _}}) ->
    {lists:member(RAlign, IAlign)};
negotiate_amr_ext({modeset, RModeset, {IModeset, _}}) ->
    case lists:filter(fun(Mode) -> lists:member(Mode, IModeset) end, RModeset) of
        [] ->
            {false};
        Modeset ->
            {true, Modeset}
    end.

negotiate_evs_ext({rate, 16000}) ->
    true;
negotiate_evs_ext({rate, _}) ->
    false;
negotiate_evs_ext({channel, Ch}) when Ch =:= 1; Ch =:= none ->
    true;
negotiate_evs_ext({channel, _}) ->
    false;
negotiate_evs_ext({mode_switch, 0}) ->
    true;
negotiate_evs_ext({mode_switch, _}) ->
    false;
negotiate_evs_ext({bitrate, BitRate}) ->
    lists:member(BitRate, ?EvsBitrate).

negotiate_video_ext({rate, 90000}) ->
    {true};
negotiate_video_ext({rate, _}) ->
    {false}.

negotiate_sendrecv(sendrecv) ->
    sendrecv;
negotiate_sendrecv(sendonly) ->
    recvonly;
negotiate_sendrecv(recvonly) ->
    sendonly;
negotiate_sendrecv(none) ->
    none.

deal_negotype(Type, Negosdp) when Type =:= none; Type =:= {common} ->
    {ok, Negosdp};
deal_negotype(Type, #sdp{audio = #media_descript{port = PortA, codeclist = AuCodeclist, digit = Digit}
                       , video = #media_descript{port = PortV, codeclist = ViCodeclist}} = Negosdp) when Type =:= {remote} ->
    case {PortA, PortV} of
        {0, _} ->
            [Vifirst|_] = ViCodeclist,
            {ok, Negosdp#sdp{video = (Negosdp#sdp.video)#media_descript{codeclist = [Vifirst]}}};
        {_, 0} ->
            case {AuCodeclist, Digit} of
                {[Aufirst|_], []} ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst]}}};
                {[#codec{type = Codec} = Aufirst|_], [#codec{ext = #ext{rate = 8000}} = Difirst|_]} when Codec =:= alaw;
                                                                                                         Codec =:= ulaw;
                                                                                                         Codec =:= amrnb ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst], digit = [Difirst]}}};
                {[#codec{type = Codec} = Aufirst|_], [#codec{ext = #ext{rate = 16000}} = Difirst|_]} when Codec =:= amrwb;
                                                                                                          Codec =:= evs ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst], digit = [Difirst]}}};
                {_, [_|Direst]} ->
                    deal_negotype(Type, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{digit = Direst}})
            end;
        {_, _} ->
            case {AuCodeclist, ViCodeclist, Digit} of
                {[Aufirst|_], [], []} ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst]}}};
                {[#codec{type = Codec} = Aufirst|_], [], [#codec{ext = #ext{rate = 8000}} = Difirst|_]} when Codec =:= alaw;
                                                                                                             Codec =:= ulaw;
                                                                                                             Codec =:= amrnb ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst], digit = [Difirst]}}};
                {[#codec{type = Codec} = Aufirst|_], [], [#codec{ext = #ext{rate = 16000}} = Difirst|_]} when Codec =:= amrwb;
                                                                                                              Codec =:= evs ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst], digit = [Difirst]}}};
                {_, [], [_|Direst]} ->
                    deal_negotype(Type, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{digit = Direst}});
                {[Aufirst|_], [Vifirst|_], []} ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst]}
                                    ,video = (Negosdp#sdp.video)#media_descript{codeclist = [Vifirst]}}};
                {[#codec{type=Codec}=Aufirst|_], [Vifirst|_], [#codec{ext=#ext{rate=8000}}=Difirst|_]} when Codec =:= alaw;
                                                                                                            Codec =:= ulaw;
                                                                                                            Codec =:= amrnb ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst], digit = [Difirst]}
                                    ,video = (Negosdp#sdp.video)#media_descript{codeclist = [Vifirst]}}};
                {[#codec{type = Codec} = Aufirst|_], [Vifirst|_],
                 [#codec{ext = #ext{rate = 16000}} = Difirst|_]} when Codec =:= amrwb;
                                                                      Codec =:= evs ->
                    {ok, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{codeclist = [Aufirst], digit = [Difirst]}
                                    ,video = (Negosdp#sdp.video)#media_descript{codeclist = [Vifirst]}}};
                {_, _, [_|Direst]} ->
                    deal_negotype(Type, Negosdp#sdp{audio = (Negosdp#sdp.audio)#media_descript{digit = Direst}})
            end
    end;
deal_negotype({local, Prilist}, #sdp{audio = #media_descript{port = PortA, codeclist = AuCodeclist}
                                   , video = #media_descript{port = PortV, codeclist = ViCodeclist}} = Negosdp) ->
    {{audio, Auprilist}, {video, Viprilist}} = Prilist,
    Ins = case {PortA, PortV} of
        {0, _} ->
            [{video, ViCodeclist, Viprilist}];
        {_, 0} ->
            [{audio, AuCodeclist, Auprilist}];
        {_, _} ->
            [{audio, AuCodeclist, Auprilist}, {video, ViCodeclist, Viprilist}]
    end,
    {ok, lists:foldl(fun(I, Sdp) -> set_sdp_by_local(I, Sdp) end, Negosdp, Ins)}.

set_sdp_by_local({Mediatype, Codeclist, Prilist}, Sdp) ->
    case lists:foldl(fun(Codecname, R) ->
                            get_codec_by_prilist(Codecname, R) end
                      , {Mediatype, {Codeclist, none}, []}, Prilist) of
        {_, _, []} ->
            Sdp;
        {audio, _, Aulist} ->
            Digit = set_digit_by_codec(Aulist, (Sdp#sdp.audio)#media_descript.digit),
            Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = Aulist, digit = Digit}};
        {video, _, Vilist} ->
            Sdp#sdp{video = (Sdp#sdp.video)#media_descript{codeclist = Vilist}}
    end.

get_codec_by_prilist(Codecname, {Mediatype, {Codeclist, Para}, []}) ->
    case lists:keyfind(Codecname, 2, Codeclist) of
        false when Codecname =:= amrnb_oct; Codecname =:= amrnb_eff;
                   Codecname =:= amrwb_oct; Codecname =:= amrwb_eff ->
            [NCodecname, Align] = string:tokens(atom_to_list(Codecname), "_"),
            get_codec_by_prilist(list_to_atom(NCodecname), {Mediatype, {Codeclist, list_to_atom(Align)}, []});
        false ->
            {Mediatype, {Codeclist, none}, []};
        Codec when Para =:= none ->
            {Mediatype, {Codeclist, none}, [Codec]};
        Codec ->
            case (Codec#codec.ext)#ext.align of
                A when A =:= Para ->
                    {Mediatype, {Codeclist, none}, [Codec]};
                _ ->
                    {Mediatype, {Codeclist, none}, []}
            end
    end;
get_codec_by_prilist(_, R) ->
    R.

set_digit_by_codec(_, []) ->
    [];
set_digit_by_codec([#codec{type = Codec}], [#codec{ext = #ext{rate = 8000}}= Digit|_]) when Codec =:= ulaw;
                                                                                            Codec =:= alaw;
                                                                                            Codec =:= amrnb ->
    [Digit];
set_digit_by_codec([#codec{type = Codec}], [#codec{ext = #ext{rate = 16000}}= Digit|_]) when Codec =:= amrwb;
                                                                                             Codec =:= evs ->
    [Digit];
set_digit_by_codec(Codec, [_|Rest]) ->
    set_digit_by_codec(Codec, Rest).

parse_remote_sdp(asn1_NOVALUE) ->
    {ok, none};
parse_remote_sdp([]) ->
    {ok, none};
parse_remote_sdp(StrRemoteSdp) ->
    case lists:foldl(fun(Line, {Scope, Result}) -> parse_str_sdp(Line, {Scope, Result})
                          end
                          , {session, []}
                          , string:tokens(StrRemoteSdp, "\r\n")) of
        {session, Lines} ->
            case lists:foldl(fun(SdpLine, Sdp) ->
                                     parse_remote_sdp_line(SdpLine, Sdp) end
                                 , #sdp{}
                                 , lists:reverse(Lines)) of
                {error, R} ->
                    {error, R};
                #sdp{audio = #media_descript{codeclist = []}, video = #media_descript{codeclist = []}} ->
                    {error, "remoteSdp error: both aucodeclist and vicodeclist are empty"};
                #sdp{audio = #media_descript{codeclist = []}, video = #media_descript{port = P}} when P =:= 0; P =:= none ->
                    {error, "remoteSdp error: aucodeclist is empty and viport is zero"};
                #sdp{audio = #media_descript{port = P}, video = #media_descript{codeclist = []}} when P =:= 0; P =:= none ->
                    {error, "remoteSdp error: vicodeclist is empty and auport is zero"};
                #sdp{audio = #media_descript{port = 0}, video = #media_descript{port = P}} when P =:= 0; P =:= none ->
                    {error, "remoteSdp error: both auport and viport are zero"};
                #sdp{audio = #media_descript{port = none}, video = #media_descript{port = P}} when P =:= 0; P =:= none ->
                    {error, "remoteSdp error: both auport and viport are zero"};
                #sdp{audio = #media_descript{port = APort, addr = IP1}
                   , video = #media_descript{port = VPort, addr = IP2}} when IP1 =/= IP2,
                                                                             VPort =/= none, APort =/= none ->
                    {error, "remoteSdp error: audio addr unequal to video addr"};
                RemoteSdp ->
                    {ok, RemoteSdp}
            end;
        Error ->
            Error
    end.

parse_str_sdp(_, {error, _} = E) ->
    E;
parse_str_sdp(StrSdpline, {session, Result}) ->
    Type = string:substr(StrSdpline, 1, 1),
    Content = string:substr(StrSdpline, 3),
    case string:strip(Content, both, $ ) of
        NContent when length(Type) =:= 1, length(NContent) =/= 0 ->
            [Mediatype|_] = string:tokens(NContent, " "),
            NScope = case Type of
                "m" when Mediatype =:= ?AUDIO ->
                    audio;
                "m" when Mediatype =:= ?VIDEO ->
                    video;
                _ when length(Result) =:= 0 ->
                    session;
                _ ->
                    [{Scope, _}|_] = Result,
                    Scope
            end,
            case string:tokens(Mediatype, ":") of
                ["rtpmap"|[Payload]] when NScope =:= video ->
                    NResult = [{rtpmap, Payload}|Result],
                    {session, [{NScope, {Type, string:tokens(NContent, " ")}}|NResult]};
                ["framerate"|[Framerate]] ->  %%要求framerate在rtpmap之后，根据相应的rtpmap来确定它是哪个编码的描述
                    case lists:keyfind(rtpmap, 1, Result) of  %%如果framerate之前没有rtpmap则认为是h263的framerate
                        false ->                              %%当h263编码在h264之后且h263没有rtpmap，此时若有一个framerate无论
                            {session, [{NScope, {Type, ["framerate:34", Framerate]}}|Result]}; %%是想描述谁都会算在h264的头上
                        {rtpmap, Payload} ->
                            NResult = lists:delete({rtpmap, Payload}, Result),
                            {session, [{NScope, {Type, [string:join(["framerate", Payload], ":"), Framerate]}}|NResult]}
                    end;
                _ ->
                    {session, [{NScope, {Type, string:tokens(NContent, " ")}}|Result]}
            end;
        _ ->
            {error, "remoteSdp error: sdp value error"}
    end.

parse_remote_sdp_line(_, {error, _} = E) ->
    E;
parse_remote_sdp_line({rtpmap, _}, Sdp) ->
    Sdp;
parse_remote_sdp_line({Scope, {Type, Value}}, Sdp) ->
    case Type of
        "v" ->
            parse_proto_ver(Value, Sdp);
        "o" ->
            parse_origin(Value, Sdp);
        "c" ->
            parse_connection(Scope, Value, Sdp);
        "m" ->
            parse_media(Value, Sdp); %%格式检查做到m行
        "a" ->
            parse_attribute(Scope, Value, Sdp);
        _ ->
            %%info_manager:info("unparse sdp line ~p~n", [Scope, {Type, Value}}]),
            Sdp
    end.

parse_proto_ver([Ver], Sdp) ->
    case catch list_to_integer(Ver) of
        {'EXIT', _} ->
            {error, "remoteSdp error: proto-version value error"};
        _ ->
            Sdp
    end;
parse_proto_ver(_, _) ->
    {error, "remoteSdp error: proto-version format error"}.

parse_origin(Value, _) when length(Value) =/= 6 ->
    {error, "remoteSdp error: origin-field format error"};
parse_origin([Username, Sessid, Sessver, Nettype, Addrtype, Address], Sdp) ->
    lists:foldl(fun(I, R) -> parse_ori_value(I, R) end, Sdp, [{username, Username}, {sessid, Sessid},
                                                              {sessver, Sessver}  , {nettype, Nettype},
                                                              {address, {Addrtype, Address}}]).

parse_ori_value(_, {error, _} = E) ->
    E;
parse_ori_value({username, Username}, Sdp) ->
    case lists:all(fun(I) -> is_visible_ch(I) end, Username) of
        true ->
            Sdp;
        false ->
            {error, "remoteSdp error: origin username value error"}
    end;
parse_ori_value({sessid, Sessid}, Sdp) ->
    case catch list_to_integer(Sessid) of
        {'EXIT', _} ->
            {error, "remoteSdp error: origin sess-id value error"};
        _ ->
            Sdp
    end;
parse_ori_value({sessver, Sessver}, Sdp) ->
    case catch list_to_integer(Sessver) of
        {'EXIT', _} ->
            {error, "remoteSdp error: origin sess-version value error"};
        _ ->
            Sdp
    end;
parse_ori_value({nettype, ?NET}, Sdp) ->
    Sdp;
parse_ori_value({nettype, _}, _) ->
    {error, "remoteSdp error: origin nettype value error"};
parse_ori_value({address, {_Addrtype, _Address}}, Sdp) ->
    Sdp.
    %%case parse_address({Addrtype, Address}) of
    %%    {error, R} ->
    %%        {error, R};
    %%    {_NAddrtype, {ok, _IP}} ->
    %%        Sdp;
    %%    {_, {error, einval}} ->
    %%        {error, "remoteSdp error: origin address value error"}
    %%end.

is_visible_ch(Ch) when Ch > 32, Ch < 127 ->   %% visible characters: %x21-7E
    true;
is_visible_ch(Ch) when Ch > 127, Ch < 256 ->  %% expand visible ch: %80-FF
    true;
is_visible_ch(_) ->
    false.

parse_connection(_, Value, _) when length(Value) =/= 3 ->
    {error, "remoteSdp error: connection-field format error"};
parse_connection(_, [Nettype, _, _], _) when Nettype =/= ?NET ->
    {error, "remoteSdp error: connection nettype value error"};
parse_connection(Scope, [_, Addrtype, Address], Sdp) ->
    case parse_address({Addrtype, Address}) of
        {error, R} ->
            {error, R};
        {NAddrtype, {ok, IP}} when Scope =:= session ->
            Sdp#sdp{addrtype = NAddrtype, addr = IP
                  , audio = (Sdp#sdp.audio)#media_descript{addrtype = NAddrtype, addr = IP}
                  , video = (Sdp#sdp.video)#media_descript{addrtype = NAddrtype, addr = IP}};
        {NAddrtype, {ok, IP}} when Scope =:= audio ->
            Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{addrtype = NAddrtype, addr = IP}};
        {NAddrtype, {ok, IP}} when Scope =:= video ->
            Sdp#sdp{video = (Sdp#sdp.video)#media_descript{addrtype = NAddrtype, addr = IP}};
        {_, {error, einval}} ->
            {error, "remoteSdp error: connection address value error"}
    end.

parse_address({?IP4, Address}) ->
    {inet, inet:parse_ipv4strict_address(Address)};
parse_address({?IP6, Address}) ->
    {inet6, inet:parse_ipv6strict_address(Address)};
parse_address({_, _}) ->
    {error, "remoteSdp error: origin/connection addrtype value error"}.

parse_media(Value, _) when length(Value) < 4 ->
    {error, "remoteSdp error: media-description format error"};
parse_media(Value, Sdp) ->
    %{[Mediatype, Port, Proto], Payloads} = lists:split(3, Value),
    [Mediatype|[Port|[Proto|Payloads]]] = Value,
    case Mediatype of
        ?APP ->
            Sdp#sdp{mediatype = Mediatype
                  , app = [#media_descript{port = 0, proto = Proto, codeclist = Payloads}|Sdp#sdp.app]};
        ?TEXT ->
            Sdp#sdp{mediatype = Mediatype
                  , text = [#media_descript{port = 0, proto = Proto, codeclist = Payloads}|Sdp#sdp.text]};
        _ when Mediatype =:= ?AUDIO; Mediatype =:= ?VIDEO ->
            case lists:foldl(fun(I, R) -> parse_media_value(I, R) end, [], [{port, Port},{proto, Proto},{fmt, Payloads}]) of
                [{fmt, NPayloads}, {port, NPort}] ->
                    Codeclist = lists:map(fun(PT) -> parse_codec(PT) end, NPayloads),
                    case Mediatype of
                        ?AUDIO ->
                            Sdp#sdp{mediatype = Mediatype
                                  , audio = (Sdp#sdp.audio)#media_descript{port = NPort, codeclist = Codeclist}};
                        ?VIDEO ->
                            Sdp#sdp{mediatype = Mediatype
                                  , video = (Sdp#sdp.video)#media_descript{port = NPort, codeclist = Codeclist}}
                    end;
                E ->
                    E
            end;
        _Unsupport ->
            {error, "remoteSdp error: media_type value error"}
    end.

parse_media_value(_, {error, _} = E) ->
    E;
parse_media_value({port, Port}, R) ->
    case catch list_to_integer(Port) of
        {'EXIT', _} ->
            {error, "remoteSdp error: media_port value error"};
        Int ->
            [{port, Int}|R]
    end;
parse_media_value({proto, ?PROTO}, R) ->
    R;
parse_media_value({proto, "RTP/AVPF"}, R) ->
    R;
parse_media_value({proto, "udp"}, _) ->
    {error, "remoteSdp error: media_proto value not support"};
parse_media_value({proto, _}, _) ->
    {error, "remoteSdp error: media_proto value error"};
parse_media_value({fmt, Payloads}, R) ->
    NPayloads = lists:map(fun(P) -> catch list_to_integer(P) end, Payloads),
    case lists:all(fun(I) -> is_integer(I) end, NPayloads) of
        true ->
            [{fmt, NPayloads}|R];
        false ->
            {error, "remoteSdp error: media_fmt value error"}
    end.

parse_codec(Payload) ->
    case Payload of
        N when N =:= 0 ->
            #codec{type = ulaw, payload = N};
        N when N =:= 8 ->
            #codec{type = alaw, payload = N};
        N when N =:= 34 ->
            #codec{type = h263, payload = N, ext = #ext{rate = 90000}};
        N ->
            #codec{type = none, payload = N}
    end.

parse_attribute(_, _, #sdp{mediatype = Type} = Sdp) when Type =:= ?APP; Type =:= ?TEXT ->
    Sdp;
parse_attribute(Scope, Value, Sdp) ->
    [AttrPayload|AttrValue] = Value,
    case catch get_attribute_value(AttrPayload) of
        {'EXIT', _R} ->
            {error, "remoteSdp error: attribute value error"};
        {Attribute, none} when Attribute =:= sendrecv; Attribute =:= sendonly; Attribute =:= recvonly ->
            parse_sendrecv(Scope, Attribute, Sdp);
        {ptime, PTime} ->
            parse_ptime(Scope, PTime, Sdp);
        {maxptime, Maxptime} ->
            parse_maxptime(Scope, Maxptime, Sdp);
        {framesize, Payload} ->
            parse_framesize(Payload, AttrValue, Sdp);
        {framerate, Payload} ->
            parse_framerate(Payload, AttrValue, Sdp);
        {rtpmap, Payload} ->
            parse_rtpmap(Scope, Payload, AttrValue, Sdp);
        {fmtp, Payload} ->
            parse_fmtp(Scope, Payload, AttrValue, Sdp);
        _ ->
            Sdp
    end.

get_attribute_value(AttrPayload) ->
    ColonPos = string:str(AttrPayload, ":"),
    {Attr, Value} = case ColonPos of
        0 ->
            {AttrPayload, none};
        _ ->
            {string:substr(AttrPayload, 1, ColonPos - 1), string:substr(AttrPayload, ColonPos + 1)}
    end,
    case Attr of
        "rtpmap" ->
            {rtpmap, list_to_integer(Value)};
        "fmtp" ->
            {fmtp, list_to_integer(Value)};
        "ptime" ->
            {ptime, list_to_integer(Value)};
        "maxptime" ->
            {maxptime, list_to_integer(Value)};
        "framerate" ->
            {framerate, list_to_integer(Value)};
        "framesize" ->
            {framesize, list_to_integer(Value)};
        "sendrecv" ->
            {sendrecv, none};
        "sendonly" ->
            {sendonly, none};
        "recvonly" ->
            {recvonly, none};
        _ ->
            {Attr, Value}
    end.

parse_sendrecv(audio, sendrecv, Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{sendrecv = sendrecv}};
parse_sendrecv(video, sendrecv, Sdp) ->
    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{sendrecv = sendrecv}};
parse_sendrecv(session, sendrecv, Sdp) ->
    Sdp#sdp{sendrecv_mark = true};
parse_sendrecv(audio, sendonly, Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{sendrecv = sendonly}};
parse_sendrecv(video, sendonly, Sdp) ->
    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{sendrecv = sendonly}};
parse_sendrecv(session, sendonly, Sdp) ->
    Sdp#sdp{sendrecv_mark = true, sendrecv = sendonly};
parse_sendrecv(audio, recvonly, Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{sendrecv = recvonly}};
parse_sendrecv(video, recvonly, Sdp) ->
    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{sendrecv = recvonly}};
parse_sendrecv(session, recvonly, Sdp) ->
    Sdp#sdp{sendrecv_mark = true, sendrecv = recvonly}.

parse_ptime(audio, Value, Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{ptime = Value}};
parse_ptime(video, Value, Sdp) ->
    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{ptime = Value}}.

parse_maxptime(audio, Value, Sdp) ->
    Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{maxptime = Value}};
parse_maxptime(video, Value, Sdp) ->
    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{maxptime = Value}}.

parse_framesize(Payload, [Framesize], #sdp{video = #media_descript{codeclist = ViCodeclist}} = Sdp) ->
    case lists:keyfind(Payload, 3, ViCodeclist) of
        false ->
            {error, "remoteSdp error: attribute framesize payload error"};
        Codec ->
            case catch get_codec_by_framesize(Framesize, Codec) of
                {'EXIT', _R} ->
                    {error, "remoteSdp error: attribute framesize value error"};
                NCodec ->
                    NViCodeclist = lists:keyreplace(Payload, 3, ViCodeclist, NCodec),
                    Sdp#sdp{video = (Sdp#sdp.video)#media_descript{codeclist = NViCodeclist}}
            end
    end.

get_codec_by_framesize(Framesize, {codec, _, _, Ext} = Codec) ->
    [Width, Height] = string:tokens(Framesize, "-"),
    Codec#codec{ext = Ext#ext{framesize = {list_to_integer(Width), list_to_integer(Height)}, framesize_mark = true}}.

parse_framerate(Payload, [Framerate], #sdp{video = #media_descript{codeclist = ViCodeclist}} = Sdp) ->
    Codec = lists:keyfind(Payload, 3, ViCodeclist),
    {Result, NFramerate} = case catch list_to_integer(Framerate) of
                                FR_int when is_integer(FR_int) ->
                                    {ok, FR_int};
                                {'EXIT', _} ->
                                    case catch list_to_float(Framerate) of
                                        FR_float when is_float(FR_float) ->
                                            {ok, FR_float};
                                        {'EXIT', _} ->
                                            {error, "remoteSdp error: attribute framerate value error"}
                                    end
                            end,
    case {Result, NFramerate} of
        {ok, NFramerate} ->
            NCodec = Codec#codec{ext = (Codec#codec.ext)#ext{framerate = NFramerate, framerate_mark = true}},
            NViCodeclist = lists:keyreplace(Payload, 3, ViCodeclist, NCodec),
            Sdp#sdp{video = (Sdp#sdp.video)#media_descript{codeclist = NViCodeclist}};
        Error ->
            Error
    end.

parse_rtpmap(Scope, Payload, [Rtpmap], #sdp{audio = #media_descript{codeclist = AuCodeclist, digit = Digit}
                                           ,video = #media_descript{codeclist = ViCodeclist}} = Sdp) ->
    NCodeclist = case Scope of
                    audio ->
                        lists:append([AuCodeclist, Digit]);
                    video ->
                        ViCodeclist;
                    _ ->
                        []
                 end,
    case lists:keyfind(Payload, 3, NCodeclist) of
        false ->
            Sdp;
        {codec, _, _, Ext} ->
            case catch get_codec_by_rtpmap(Rtpmap, Payload, Ext) of
                {codec, Codecname, _, NExt} ->
                    NSdp = set_codec(rtpmap, Scope, {codec, Codecname, Payload, NExt#ext{others = []}}, Sdp),
                    case NExt#ext.others of
                        [] ->
                            NSdp;
                        Fmtp ->
                            parse_fmtp(Scope, Payload, Fmtp, NSdp)
                    end;
                {'EXIT', _R} ->
                    {error, "remoteSdp error: attribute rtpmap value error"}
            end
    end.

get_codec_by_rtpmap(Rtpmap, Payload, Ext) ->
    [Codecname|[Second|Third]] = string:tokens(Rtpmap, "/"),
    Rate = list_to_integer(Second),
    Channel = case Third of
        [] ->
            none;
        [Value] ->
            list_to_integer(Value)
    end,
    case string:to_lower(Codecname) of
        "pcmu" ->
            {codec, ulaw, Payload, Ext#ext{rate = Rate, channel = Channel}};
        "pcma" ->
            {codec, alaw, Payload, Ext#ext{rate = Rate, channel = Channel}};
        "g723" ->
            {codec, g723, Payload, Ext#ext{rate = Rate, channel = Channel}};
        "g729" ->
            {codec, g729, Payload, Ext#ext{rate = Rate, channel = Channel}};
        "evs" ->
            {codec, evs, Payload, Ext#ext{rate = Rate, channel = Channel}};
        "amr" ->
            {codec, amrnb, Payload, Ext#ext{rate = Rate, channel = Channel, modeset = [0,1,2,3,4,5,6,7]}};
        "amr-wb" ->
            {codec, amrwb, Payload, Ext#ext{rate = Rate, channel = Channel, modeset = [0,1,2,3,4,5,6,7,8]}};
        "telephone-event" ->
            {codec, telephone, Payload, Ext#ext{rate = Rate, channel = Channel}};
        "h264" ->
            {codec, h264, Payload, Ext#ext{rate = Rate, channel = Channel}};
        "h263" ->
            {codec, h263, Payload, Ext#ext{rate = Rate, channel = Channel}};
        _ ->
            {codec, list_to_atom(Codecname), Payload, Ext}
    end.

set_codec(Type, Scope, {_, Codecname, Payload, _} = Codec, #sdp{audio = #media_descript{codeclist = AuCodeclist, digit = Digit}
                                                               ,video = #media_descript{codeclist = ViCodeclist}} = Sdp) ->
    case Scope of
        audio ->
            {NAuCodeclist, NDigit} = case Codecname of
                telephone when Type =:= rtpmap ->
                    {lists:keydelete(Payload, 3, AuCodeclist), lists:append(Digit, [Codec])};
                telephone when Type =:= fmtp ->
                    {AuCodeclist, lists:keyreplace(Payload, 3, Digit, Codec)};
                _ when Type =:= rtpmap; Type =:= fmtp ->
                    {lists:keyreplace(Payload, 3, AuCodeclist, Codec), Digit}
            end,
            Sdp#sdp{audio = (Sdp#sdp.audio)#media_descript{codeclist = NAuCodeclist, digit = NDigit}};
        video ->
            Sdp#sdp{video = (Sdp#sdp.video)#media_descript{codeclist = lists:keyreplace(Payload, 3, ViCodeclist, Codec)}};
        _ ->
            Sdp
    end.

parse_fmtp(Scope, Payload, Fmtp, #sdp{audio = #media_descript{codeclist = AuCodeclist, digit = Digit}
                                     ,video = #media_descript{codeclist = ViCodeclist}} = Sdp) ->
    NCodeclist = case Scope of
                    audio ->
                        lists:append([AuCodeclist, Digit]);
                    video ->
                        ViCodeclist;
                    _ ->
                        []
                 end,
    case lists:keyfind(Payload, 3, NCodeclist) of
        false ->
            Sdp;
        {codec, Codecname, _, Ext} ->
            case catch get_codec_by_fmtp(Codecname, Payload, Ext, Fmtp) of
                {'EXIT', _R} ->
                    {error, "remoteSdp error: attribute fmtp value error"};
                Codec ->
                    set_codec(fmtp, Scope, Codec, Sdp)
            end
    end.

get_codec_by_fmtp(none, Payload, Ext, Fmtp) ->
    {codec, none, Payload, Ext#ext{others = Fmtp}};
get_codec_by_fmtp(Codecname, Payload, Ext, Fmtp) ->
    NFmtp = string:join(Fmtp, " "),  %%防止fmtp里包含空格
    NExt = lists:foldr(fun(Para, Paras) -> parse_fmtp_atom(Para, Paras) end, Ext, string:tokens(NFmtp, "; ")),
    {codec, Codecname, Payload, NExt}.

parse_fmtp_atom(FmtpPara, #ext{others = Others} = Paras) ->
    EqualPos = string:str(FmtpPara, "="),
    {ParaName, Value} = case EqualPos of
        0 ->
            {FmtpPara, none};
        _ ->
            {string:substr(FmtpPara, 1, EqualPos - 1), string:substr(FmtpPara, EqualPos + 1)}
    end,
    case ParaName of
        "octet-align" ->
            case list_to_integer(Value) of
                1 ->
                    Paras#ext{align_mark = true, align = oct};
                0 ->
                    Paras#ext{align_mark = true, align = eff}
            end;
        "mode-set" ->
            Paras#ext{modeset_mark = true
                     ,modeset = lists:map(fun(I) -> list_to_integer(I) end, string:tokens(Value, ","))};
        "evs-mode-switch" ->
            case list_to_integer(Value) of
                1 ->
                    Paras#ext{mode_switch = 1};
                0 ->
                    Paras#ext{mode_switch = 0}
            end;
        "br" ->
            Paras#ext{bitrate = get_max_bitrate(Value)};
        _ when Value =/= none ->  %%ParaName=Value(octet-align和modeset除外)
            Paras#ext{others = [FmtpPara|Others]};
        _ ->  %%FmtpPara中不包含=的都会走到这里
            Paras#ext{others = [FmtpPara|Others]}
    end.

get_max_bitrate(Bitrate) ->
    case string:str(Bitrate, "-") of
        0 ->
            Bitrate;
        HyphenPos ->
            string:substr(Bitrate, HyphenPos+1)
    end.
