%% ** refs
%%
%% [1] http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
%% [2] http://ccrma.stanford.edu/courses/422/projects/WaveFormat/
%% [3] http://chlorophil.blogspot.com/2007/06/erlang-binary-map.html

-module(wave).
-export([parse/1, create/1]).

-include("wave.hrl").

parse(Binary) ->  %% returns wave record
    parse_chunk(#wave{}, Binary, []).

%%正常文件结尾，必要数据完整
parse_chunk(#wave{ wformat_code = WFormatTag
                 , sample_rate = SampleRate
                 , bits_per_sample = BitsPerSample
                 , data = Data} = Wave
            , <<>>
            , Error) when WFormatTag =/= undefined,
                          SampleRate =/= undefined,
                          BitsPerSample =/= undefined,
                          Data =/= undefined ->
    {Wave, Error};
%%结尾有多余数据，必要数据完整
parse_chunk(#wave{ wformat_code = WFormatTag
                 , sample_rate = SampleRate
                 , bits_per_sample = BitsPerSample
                 , data = Data} = Wave
            , Data
            , Error) when byte_size(Data) < 8,
                          WFormatTag =/= undefined,
                          SampleRate =/= undefined,
                          BitsPerSample =/= undefined,
                          Data =/= undefined ->
    {Wave, [{garbage, Data}|Error]};
%%必要数据不完整，所以文件有错
parse_chunk(_Wave, Data, Error) when byte_size(Data) < 8 ->
    error([{bad_chunk, Data}|Error]);
%%解析chunk
parse_chunk(Wave, <<ID:4/binary, Length:32/little, Rest/binary>> = Chunk, Error) ->
    case ID of
        <<"RIFF">> when Length > 4 ->
            case Rest of
                <<"WAVE", R/binary>> ->
                    parse_chunk(Wave, R, Error);
                _ ->
                    error({bad_chunk, Chunk})
            end;
        %%ulaw or alaw
        <<"fmt ">> when Length =:= 18 ->
            << WFormatTag:16/little
	     , 1:16/little          %% NumChannels
	     , 8000:32/little       %% SampleRate
	     , _DataRate:32/little  %% DataRate = SampleRate * NumChanles * BitsPerSample / 8
	     , _BlockSize:16/little %% BlockSize = NumChannels * BitsPerSample / 8
	     , BitsPerSample:16/little
             , _CbSize:16/little
	     , R/binary>> = Rest,
            parse_chunk(Wave#wave{ sample_rate = 8000
                                 , wformat_code = WFormatTag
                                 , bits_per_sample = BitsPerSample}
                       , R
                       , Error);
        %%pcm
        <<"fmt ">> when Length =:= 16 ->
            << WFormatTag:16/little %% FormatCode
	     , 1:16/little          %% NumChannels
	     , 8000:32/little       %% SampleRate
	     , _DataRate:32/little  %% DataRate = SampleRate * NumChanles * BitsPerSample / 8
	     , _BlockSize:16/little %% BlockSize = NumChannels * BitsPerSample / 8
	     , BitsPerSample:16/little
	     , R/binary>> = Rest,
            parse_chunk(Wave#wave{ sample_rate = 8000
                                 , wformat_code = WFormatTag
                                 , bits_per_sample = BitsPerSample}
                       , R
                       , Error);
        %%fact数据不必要，也不取值
        <<"fact">> ->
            <<_Data:Length/binary, R/binary>> = Rest,
            parse_chunk(Wave, R, Error);
        %%以data的实际长度为准，为了某些兼容性
        <<"data">> when Length > byte_size(Rest) ->
            parse_chunk(Wave#wave{data = Rest}, <<>>, [{bad_chunk, ID}| Error]);
        %%兼容结尾没有填充位
        <<"data">> when Length rem 2 =:= 1, (Length+1) > byte_size(Rest) ->
            parse_chunk(Wave#wave{data = Rest}, <<>>, [{bad_chunk, ID}| Error]);
        <<"data">> when Length rem 2 =:= 1 ->
            <<Data:Length/binary, _Padding:1/binary, R/binary>> = Rest,
            parse_chunk(Wave#wave{data = Data}, R, Error);
        <<"data">> when Length rem 2 =:= 0 ->
            <<Data:Length/binary, R/binary>> = Rest,
            parse_chunk(Wave#wave{data = Data}, R, Error);
        _ when Length > byte_size(Rest) ->
            parse_chunk(Wave, <<>>, [{unknown, ID}, {bad_chunk, Chunk}|Error]);
        _ ->
            <<_Data:Length/binary, R/binary>> = Rest,
            parse_chunk(Wave, R, [{unknown, ID}|Error])
    end.

create(#wave{ wformat_code = FormatCode
           %%, sample_rate = Rate
           , bits_per_sample = BitsPerSample
           , data = Data} = _Wave) when FormatCode =:= ?WAVE_ALAW
                                      ; FormatCode =:= ?WAVE_MULAW
                                      ; FormatCode =:= ?WAVE_PCM ->
    FmtChunk = create_fmt(FormatCode, BitsPerSample),
    FactChunk = create_fact(FormatCode, BitsPerSample, Data),
    DataChunk = create_data(Data),
    Length = 4 + byte_size(FmtChunk) + byte_size(FactChunk) + byte_size(DataChunk),
    << "RIFF"
     , Length:32/little
     , "WAVE"
     , FmtChunk/binary
     , FactChunk/binary
     , DataChunk/binary>>.

create_fmt(FormatCode, BitsPerSample) ->
    DataRate = 8000*1*BitsPerSample div 8,
    BlockSize = 1*BitsPerSample div 8,
    ExtSize = case FormatCode of
              ?WAVE_PCM -> <<>>;
              _ -> <<0:16/little>>
          end,
    << "fmt "
     , (16+byte_size(ExtSize)):32/little
     , FormatCode:16/little
     , 1:16/little         %% NumChannels
     , 8000:32/little      %% SampleRate
     , DataRate:32/little  %% DataRate = SampleRate * NumChanles * BitsPerSample / 8
     , BlockSize:16/little %% BlockSize = NumChannels * BitsPerSample / 8
     , BitsPerSample:16/little
     , ExtSize/binary>>.

create_fact(?WAVE_PCM, _BitsPerSample, _Data) ->
    <<>>;
create_fact(FormatCode, BitsPerSample, Data) when FormatCode =:= ?WAVE_ALAW
                                                ; FormatCode =:= ?WAVE_MULAW ->
    SampleNumber = byte_size(Data) div (BitsPerSample div 8),
    << "fact", 4:32/little, SampleNumber:32/little>>.

create_data(Data) when byte_size(Data) rem 2 =:= 0 ->
    <<"data", (byte_size(Data)):32/little, Data/binary>>;
create_data(Data) when byte_size(Data) rem 2 =:= 1 ->
    <<"data", (byte_size(Data)):32/little, Data/binary, 0:8/little>>.
