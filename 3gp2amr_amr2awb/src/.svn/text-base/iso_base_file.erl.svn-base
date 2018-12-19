%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 16 Apr 2014 by wanglihe <wanglihe@ebupt.com>

-module(iso_base_file).

-export([parse/1]).

-include("iso_base_file.hrl").
-define(Horizresolution, 4718592).
-define(Vertresolution, 4718592).

parse(Binary) ->
    case catch parse_box(Binary, #isofile{}, []) of
        {error, Reason} ->
            {error, Reason};
        {'EXIT', _Reason} ->
            {error, content};
        {#isofile{ ftyp = Ftyp
                 , duration = Duration} = IsoFile, Unknown} when Ftyp =/= undefined
                                                               , Duration =/= undefined ->
            %%确认必要参数存在
            {IsoFile, Unknown};
        _ ->
            {error, content}
    end.
parse_box(<<>>, IsoFile, Unknown) ->
    {IsoFile, Unknown};
%%don't forget uuid, but now not support
parse_box(<<_BoxLen:32, Box:4/binary, _/binary>> = Bin, IsoFile, Unknown)
                                        when Box =:= <<"mvhd">>
                                           ; Box =:= <<"elst">>
                                           ; Box =:= <<"mdhd">>
                                           ; Box =:= <<"hdlr">>
                                           ; Box =:= <<"smhd">>
                                           ; Box =:= <<"dref">>
                                           ; Box =:= <<"stsd">>
                                           ; Box =:= <<"stsc">>
                                           ; Box =:= <<"stsz">>
                                           ; Box =:= <<"stco">>
                                           ; Box =:= <<"stts">>
                                           ; Box =:= <<"tkhd">> ->
    parse_fullbox(Bin, IsoFile, Unknown);
parse_box(<<0:32, Box:4/binary, Content/binary>>, IsoFile, Unknown) ->
    parse_content(IsoFile, Box, Content, Unknown, {undefined, undefined});
parse_box(<<1:32, _/binary>> = Bin, IsoFile, Unknown) ->
    <<1:32, Box:4/binary, BoxLen:64, ContentRest/binary>> = Bin,
    ContentLen = BoxLen - 16,
    <<Content:ContentLen/binary, Rest/binary>> = ContentRest,
    {NIsoFile, NUnknown} = parse_content(IsoFile, Box, Content, Unknown, {undefined, undefined}),
    parse_box(Rest, NIsoFile, NUnknown);
parse_box(<<BoxLen:32, Box:4/binary, ContentRest/binary>>, IsoFile, Unknown) ->
    ContentLen = BoxLen - 8,
    <<Content:ContentLen/binary, Rest/binary>> = ContentRest,
    {NIsoFile, NUnknown} = parse_content(IsoFile, Box, Content, Unknown, {undefined, undefined}),
    parse_box(Rest, NIsoFile, NUnknown).

parse_fullbox(<<0:32, Box:4/binary, Version:8, Flags:24, Content/binary>>, IsoFile, Unknown) ->
    parse_content(IsoFile, Box, Content, Unknown, {Version, Flags});
parse_fullbox(<<1:32, _/binary>> = Bin, IsoFile, Unknown) ->
    <<1:32, Box:4/binary, BoxLen:64, Version:8, Flags:24, ContentRest/binary>> = Bin,
    ContentLen = BoxLen - 20,
    <<Content:ContentLen/binary, Rest/binary>> = ContentRest,
    {NIsoFile, NUnknown} = parse_content(IsoFile, Box, Content, Unknown, {Version, Flags}),
    parse_box(Rest, NIsoFile, NUnknown);
parse_fullbox(<<BoxLen:32, Box:4/binary, Version:8, Flags:24, ContentRest/binary>>, IsoFile, Unknown) ->
    ContentLen = BoxLen - 12,
    <<Content:ContentLen/binary, Rest/binary>> = ContentRest,
    {NIsoFile, NUnknown} = parse_content(IsoFile, Box, Content, Unknown, {Version, Flags}),
    parse_box(Rest, NIsoFile, NUnknown).

%%ftyp 必选，唯一
parse_content(IsoFile, <<"ftyp">>, <<MajorBrand:4/binary, MinorVersion:32, Rest/binary>>, Unknown, _) when IsoFile#isofile.ftyp =:= undefined ->
    CompatibleBrands = split_brand(Rest),
    {IsoFile#isofile{ftyp = {MajorBrand, MinorVersion, CompatibleBrands}}, Unknown};
parse_content(IsoFile, Box, Content, Unknown, _) when Box =:= <<"moov">>
                                                    ; Box =:= <<"minf">>
                                                    ; Box =:= <<"dinf">>
                                                    ; Box =:= <<"edts">>
                                                    ; Box =:= <<"mdia">>
                                                    ; Box =:= <<"stbl">> ->
    parse_box(Content, IsoFile, Unknown);
%%boxes skiped
parse_content(IsoFile, Box, _Content, Unknown, _) when Box =:= <<"free">>
                                                     ; Box =:= <<"skip">>
                                                     ; Box =:= <<"mdhd">>
                                                     ; Box =:= <<"dref">>
                                                     ; Box =:= <<"tkhd">>
                                                     ; Box =:= <<"elst">>
                                                     ; Box =:= <<"stts">>
                                                     ; Box =:= <<"stss">>
                                                     ; Box =:= <<"ctts">>
                                                     ; Box =:= <<"vmhd">>
                                                     ; Box =:= <<"iods">>
                                                     ; Box =:= <<"smhd">> ->
    {IsoFile, Unknown};
%%trak
parse_content(IsoFile, <<"trak">>, Content, Unknown, _) ->
    case catch parse_box(Content, #isofile{trak = [#trak{}]}, Unknown) of
        {trak, Reason} ->
            throw({error, Reason});
        {'EXIT', _Reason} ->
            throw({error, content});
        {RIsoFile, RUnknown} ->
            Traks = IsoFile#isofile.trak,
            [Trak|_] = RIsoFile#isofile.trak,
            {IsoFile#isofile{trak = [Trak|Traks]}, RUnknown}
    end;
%%mvhd 必选，唯一
parse_content(IsoFile, <<"mvhd">>, Content, Unknown, {Version, _}) when IsoFile#isofile.duration =:= undefined ->
    case Version of
        0 ->
            << _CTime:32
             , _MTime:32
             , _TimeScale:32
             , Duration:32
             , Rest/binary>> = Content;
        1 ->
            << _CTime:64
             , _MTime:64
             , _TimeScale:32
             , Duration:64
             , Rest/binary>> = Content
     end,
     << _Rate:32
      , _Volume:16
      , _Reserved1:16
      , _Reserved2:(32*2)
      , _Matrix:(32*9)
      , _PreDefined:(32*6)
      , _NextTrackId:32>> = Rest,
    {IsoFile#isofile{duration = Duration}, Unknown};
%%tkhd
%%parse_content(IsoFile, <<"tkhd">>, Content, Unknown, {Version, _}) ->
%%    case Version of
%%        0 ->
%%            << _CTime:32
%%             , _MTime:32
%%             , _TrackID:32
%%             , _Reserved:32
%%             , _Duration:32
%%             , Rest/binary>> = Content;
%%        1 ->
%%            << _CTime:64
%%             , _MTime:64
%%             , _TrackID:32
%%             , _Reserved:32
%%             , _Duration:64
%%             , Rest/binary>> = Content
%%     end,
%%    << _Reserved1:(32*2)
%%     , _Layer:16
%%     , _AlternateGroup:16
%%     , _Volume:16
%%     , _Reserved2:16
%%     , _Matrix:(32*9)
%%     , _Width:32
%%     , _Height:32>> = Rest,
%%     {IsoFile, Unknown};
%%elst
%%parse_content(IsoFile, <<"elst">>, Content, Unknown, _) ->
%%    <<_EntryCount:32, _/binary>> = Content,
%%    {IsoFile, Unknown};
%%hdlr
parse_content(IsoFile, <<"hdlr">>, Content, Unknown, _) ->
    << _PreDefined:32
     , HeaderType:4/binary
     , _Reserved:(32*3)
     , _Name/binary>> = Content,
    [Trak|T] = IsoFile#isofile.trak,
    MediaType = case HeaderType of
        <<"soun">> ->
            sound;
        <<"vide">> ->
            video;
        _ ->
            throw({trak, not_support})
    end,
    {IsoFile#isofile{trak = [Trak#trak{media_type = MediaType}|T]}, Unknown};
%%mdat
parse_content(IsoFile, <<"mdat">>, Content, Unknown, _) ->
    {IsoFile#isofile{mdat = Content}, Unknown};
%%stsd
parse_content(IsoFile, <<"stsd">>, Content, Unknown, _) ->
    << _EntryCount:32
     , Rest/binary>> = Content,
    parse_box(Rest, IsoFile, Unknown);
parse_content(IsoFile, <<"sawb">>, _Content, Unknown, _) ->
    [Trak|T] = IsoFile#isofile.trak,
    NTrak = Trak#trak{sound_type = amrwb},
    {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
parse_content(IsoFile, <<"samr">>, _Content, Unknown, _) ->
    [Trak|T] = IsoFile#isofile.trak,
    NTrak = Trak#trak{sound_type = amrnb},
    {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
%%stts
%%parse_content(IsoFile, <<"stts">>, Content, Unknown, _) ->
%%    <<1:32, _Count:32, _TimeDelta:32>> = Content,
%%    {IsoFile, Unknown};
%%stsc
parse_content(IsoFile, <<"stsc">>, Content, Unknown, _) ->
    <<Count:32/unsigned, Rest/binary>> = Content,
    %%ISO文档管ChunkIndex叫FirstChunk，狗屁，其实就是一个chunk索引，但是如果后
    %%一个索引的SamplePerChunk和desc值是相同的，就省略了，说得虽然明白，但一点
    %%也不清晰，看好几遍也没看懂，差评，主要就是FirstChunk这个名字取的不好
    ChunkSampleCount = [{ ChunkIndex
                       , SamplePerChunk
                       , SampleDescIndex}
                            || << ChunkIndex:32/unsigned
                                , SamplePerChunk:32/unsigned
                                , SampleDescIndex:32/unsigned>> <= Rest],
    case length(ChunkSampleCount) of
        Count ->
            [Trak|T] = IsoFile#isofile.trak,
            NTrak = Trak#trak{chunk_sample_count = ChunkSampleCount},
            {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
        _ ->
            throw({error, bad_stsc})
    end;
%%stsz
parse_content(IsoFile, <<"stsz">>, Content, Unknown, _) ->
    case Content of
        <<SampleSize:32/unsigned, SampleCount:32/unsigned>> when SampleSize =/= 0 ->
            [Trak|T] = IsoFile#isofile.trak,
            NTrak = Trak#trak{ sample_size = SampleSize
                             , sample_count = SampleCount},
            {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
        <<SampleSize:32/unsigned, SampleCount:32/unsigned, Rest/binary>> when SampleSize =:= 0 ->
            EntrySizes = [EntrySize || <<EntrySize:32/unsigned>> <= Rest],
            case length(EntrySizes) of
                SampleCount ->
                    [Trak|T] = IsoFile#isofile.trak,
                    NTrak = Trak#trak{ sample_size = EntrySizes
                                     , sample_count = SampleCount},
                    {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
                _ ->
                    throw({error, bad_stsz})
            end
    end;
%%stco
parse_content(IsoFile, <<"stco">>, Content, Unknown, _) ->
    case Content of
        <<EntryCount:32/unsigned, Rest/binary>> when EntryCount =:= 1 ->
            <<Entry:32>> = Rest,
            [Trak|T] = IsoFile#isofile.trak,
            NTrak = Trak#trak{chunk_offset = Entry},
            {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
        <<EntryCount:32/unsigned, Rest/binary>> ->
            Entrys = [Entry || <<Entry:32>> <= Rest],
            case length(Entrys) of
                EntryCount ->
                    [Trak|T] = IsoFile#isofile.trak,
                    NTrak = Trak#trak{chunk_offset = Entrys},
                    {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
                _ ->
                    throw({stco,entry_count_mismatch})
            end
    end;
%% avc1
%% now we don't know Data_reference_index very well, Data_reference_index=1
%%  << Reserved:48/unsigned %it's fix 0
%%   , Data_reference_index:16/unsigned
%%   , Pre_defined:16/unsigned   %fix 0
%%   , Reserved:16/unsigned  %fix 0
%%   , Pre_defined:96/unsigned   %fix 0
%%   , ImageWidth:16/unsigned
%%   , ImageHeight:16/unsigned,
%%   , Horizresolution:32/unsigned   %fix 0x00480000
%%   , Vertresolution:32/unsigned    %fix 0x00480000
%%   , Reserved:32/unsigned  %fix 0
%%   , Frame_count:16/unsigned   %fix 1
%%   , Compressorname:256/unsigned,
%%   , Depth:16/unsigned %fix 0x0018
%%   , Pre_defined:16/unsigned   %fix -1
%%   , Rest/binary>>
parse_content(IsoFile, <<"avc1">>, Content, Unknown, _) ->
    [Trak|_T] = IsoFile#isofile.trak,
    MediaType = Trak#trak.media_type,
    case MediaType of
        video ->
            video;
        _ ->
            throw({avc1,avc1_occur_in_sound})
    end,
    case Content of
        << 0:48/unsigned    % reserved
         , 1:16/unsigned % data_reference_index
         , 0:(16+ 16+ 96)/unsigned
         , _imageWidth:16/unsigned   % width
         , _imageHeight:16/unsigned  % height
         , ?Horizresolution:32/unsigned   % horizresolution
         , ?Vertresolution:32/unsigned   % vertresolution
         , 0:32/unsigned % reserved
         , 1:16/unsigned % frame_count
         , _compressorname:256/unsigned  % compressorname
         , 24:16/unsigned    % depth
         , _:16/unsigned % pre_defined
         , Rest/binary>> ->
            parse_box(Rest, IsoFile, Unknown);
        <<0:48/unsigned, Data_reference_index:16/unsigned, _Rest/binary>> when Data_reference_index =/= 1 ->
            throw({avc1,data_reference_index_unkown});
        _ ->
            throw({avc1,unknown_structer})
    end;

parse_content(IsoFile, <<"avcC">>, Content, Unknown, _) ->
    %% now we can only support num_of_pps and num_of_sps equal 1
    case Content of
        << _version:8/unsigned
         , _profile_idc:8/unsigned
         , _profile_comp:8/unsigned
         , _level_idc:8/unsigned
         , 255:8/unsigned
         , 7:3/unsigned
         , Num_of_sps:5/unsigned
         , SpsLen:16/unsigned
         , Sps:SpsLen/binary
         , Num_of_pps:8/unsigned
         , PpsLen:16/unsigned
         , Pps:PpsLen/binary
         , _Rest/binary>> when Num_of_sps =:= 1, Num_of_pps =:= 1 ->
            [Trak|T] = IsoFile#isofile.trak,
            NTrak = Trak#trak{pps = Pps, sps = Sps},
            {IsoFile#isofile{trak = [NTrak|T]}, Unknown};
        _ ->
            throw({avcC, unknown_structer}),
            {IsoFile, Unknown}
    end;

%%{IsoFile, Unknown};
parse_content(IsoFile, Box, _Content, Unknown, _) ->
    {IsoFile, [Box|Unknown]}.

split_brand(<<>>) ->
    [];
split_brand(<<Brand:4/binary, Rest/binary>>) ->
    [Brand|split_brand(Rest)].
