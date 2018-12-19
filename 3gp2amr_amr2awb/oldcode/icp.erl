-module(icp).

-export([encode/2, decode/1]).

-include("ips_codec.hrl").


encode(alloc, {RefNum}) ->
    Command = <<RefNum:32/little  %% RefNum:32/little
              , 0:16              %% BoardSelector:16/little
              , 0:16              %% TrunkMID:16/little
              , 0:16              %% TrunkSLOT:16/little
              , 5:16/little       %% SpecializedResourceType:16/little
              , 1:8               %% BCType:8
              , 0:16              %% AssocRsSlot:16/little
              , 0:8               %% NbupMode:8
              , 4:8>>,            %% MediaAttribute:8
    gen_icp(?IPS_200_BG_A_MEDIARES_ALLOC_REQ, Command);

encode(ack, {{RefNum, IvrSlot}, Addr, Media, DC, SeqNumber}) ->
    Command_H = <<RefNum:32/little  %% RefNum:32/little
                , IvrSlot:16/little-unsigned       %% IVRSLOT:16/little
                , SeqNumber:8>>,    %% SeqNumber:8
    RtpConnectCmd = gen_sub_command({16#95, {Addr, Media, DC}}),
    Command = <<Command_H/binary
              , 16#05               %% Service_Indication:8
              , RtpConnectCmd/binary>>,
    gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command);

encode(play, {{RefNum, IvrSlot}, Filename, Times, Interval, TotalTime, SeqNumber}) ->
    Command_H = <<RefNum:32/little  %% RefNum:32/little
                , IvrSlot:16/little-unsigned      %% IVRSLOT:16/little
                , SeqNumber:8>>,    %% SeqNumber:8
    MutiMediaPlay = gen_sub_command({16#b6, {Filename, Times, Interval, TotalTime}}),
    PlayOffsetTime = gen_sub_command({16#fd, {0}}),
    Command = <<Command_H/binary
              , 16#06:8             %% Service_Indication:8
              , MutiMediaPlay/binary
              , PlayOffsetTime/binary>>,
    gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command);

encode(dc, {{RefNum, IvrSlot}, Barge, MaxTime, FDT, IDT, SeqNumber}) ->
    Command_H = <<RefNum:32/little  %% RefNum:32/little
                , IvrSlot:16/little-unsigned       %% IVRSLOT:16/little
                , SeqNumber:8>>,    %% SeqNumber:8
    RtpDigitCollection = gen_sub_command({16#b7, {Barge, MaxTime, FDT, IDT}}),
    Command = <<Command_H/binary
              , 16#06:8
              , RtpDigitCollection/binary>>,
    gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command);

encode(release, {RefNum, IvrSlot}) ->
    Command = <<RefNum:32/little  %% RefNum:32/little
              , IvrSlot:16/little-unsigned>>,           %% IVRSLOT:16/little
    gen_icp(?IPS_200_BG_A_IVRRELEASE_REQ, Command);

%%encode({service, {Type, Param, SeqNumber}}, RefNum) ->
%%    Command_H = <<RefNum:32/little  %% RefNum:32/little
%%                , 0:16/little       %% IVRSLOT:16/little
%%                , SeqNumber:8>>,    %% SeqNumber:8
%%    case Type of
%%        ack ->
%%            RtpConnectCmd = gen_sub_command({16#95, Param}),
%%            Command = <<Command_H/binary
%%                      , 16#05       %% Service_Indication:8
%%                      , RtpConnectCmd/binary>>,
%%            gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command);
%%        play ->   %% barge=false
%%            MutiMediaPlay = gen_sub_command({16#b6, Param}),
%%            PlayOffsetTime = gen_sub_command({16#fd}),
%%            Command = <<Command_H/binary
%%                      , 16#06:8     %% Service_Indication:8
%%                      , MutiMediaPlay/binary
%%                      , PlayOffsetTime/binary>>,
%%            gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command);
%%        dtmf ->
%%            RtpDigitCollection = gen_sub_command({16#b7, Param}),
%%            Command = <<Command_H/binary
%%                      , 16#06:8
%%                      , RtpDigitCollection/binary>>,
%%            gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command);
%%        aupc ->   %% barge=true
%%            {Param_PLAY, Param_DTMF} = Param,
%%            RtpDigitCollection = gen_sub_command({16#b7, Param_DTMF}),
%%            MutiMediaPlay = gen_sub_command({16#b6, Param_PLAY}),
%%            Command = <<Command_H/binary
%%                      , 16#06:8
%%                      , RtpDigitCollection/binary
%%                      , MutiMediaPlay/binary>>,
%%            gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command);
%%        rec ->
%%            RtpRecordParameter = gen_sub_command({16#b8, Param}),
%%            Command = <<Command_H/binary
%%                      , 16#06:8
%%                      , RtpRecordParameter/binary>>,
%%            gen_icp(?IPS_200_BG_A_IVRSERVICE_REQ, Command)
%%    end;
%%
encode(create, RefNum) ->
    AssocResource = gen_sub_command({16#e9, RefNum}),
    Command = <<RefNum:32/little  %% RefNum:32/little
              , 0:16              %% ConfReserveRes:16/little
              , 16#34:8           %% ConfNature:8
              , 1:8               %% videoConfAttri:8
              , 0:32              %% Reserve:32/little
              , AssocResource/binary>>,
    gen_icp(?IPS_200_BG_A_IPCONFCREATE_REQ, Command);

encode({destory, ConfRoomID}, RefNum) ->
    Command = <<RefNum:32/little         %% RefNum:32/little
              , ConfRoomID:16/little>>,  %% ConfRoomID:16/little
    gen_icp(?IPS_200_BG_A_IPCONFDELETE_REQ, Command);

encode({control, {Type, {ConfRoomID, Param}, SeqNumber}}, ConfRefNum) ->
    Command_H = <<ConfRefNum:32/little   %% RefNum:32/little
                , ConfRoomID:16/little   %% ConfRoomID:16/little
                , SeqNumber:8>>,         %% SeqNumber:8
    case Type of
        start_rec ->
            {FileName} = Param,
            IpConfRecPara = gen_sub_command({16#7b, {start, FileName ++ [0]}}),
            Command = <<Command_H/binary
                      , 16#09:8      %% OperationIndication:9
                      , IpConfRecPara/binary>>,
            gen_icp(?IPS_200_BG_A_IPCONFCONTROL_REQ, Command);
        stop_rec ->
            IpConfRecPara = gen_sub_command({16#7b, {stop, []}}),
            Command = <<Command_H/binary
                      , 16#09:8      %% OperationIndication:9
                      , IpConfRecPara/binary>>,
            gen_icp(?IPS_200_BG_A_IPCONFCONTROL_REQ, Command);
        join ->
            {ConfMembID, MembRefNum} = Param,
            IpIvrResAddIn = gen_sub_command({16#7d, {ConfMembID, MembRefNum}}),
            Command = <<Command_H/binary
                      , 16#0C:8      %% OperationIndication:12
                      , IpIvrResAddIn/binary>>,
            gen_icp(?IPS_200_BG_A_IPCONFCONTROL_REQ, Command);
        unjoin ->
            {ConfMembID, MembRefNum} = Param,
            IpConfMemResID = gen_sub_command({16#7f, MembRefNum}),
            Command = <<Command_H/binary
                      , 16#0D:8      %% OperationIndication:13
                      , IpConfMemResID/binary>>,
            gen_icp(?IPS_200_BG_A_IPCONFCONTROL_REQ, Command)
    end.

decode(Bin) ->
    <<_:16/little
    , _ICP_H:8/binary
    , Frame/binary>> = Bin,

    <<_FRAME_H:12/binary
    , Operation:32/little
    , FrameBody/binary>> = Frame,

    <<BodyLength:32/little
    , Command/binary>> = FrameBody,

    case Operation of
        ?IPS_200_BG_A_MEDIARES_ALLOC_CNF ->
            <<RefNum:32/little, IvrSlot:16/little-unsigned, AllocResult:16/little, RtpParatmeter/binary>> = Command,
            <<16#94, _Length:8, IP4:8, IP3:8, IP2:8, IP1:8, MediaPort:16/little, _/binary>> = RtpParatmeter,
            {alloc, AllocResult, {RefNum, IvrSlot},{{IP1, IP2, IP3, IP4}, MediaPort}};
        ?IPS_200_BG_A_IVRSERVICE_CNF ->
            case BodyLength of
                16#0A ->
                    <<RefNum:32/little, _:3/binary, _Service_Id:8, Result:16/little>> = Command;
                _ ->
                    <<RefNum:32/little, _:3/binary, 16#06:8, Result:16/little, _/binary>> = Command
                end,
                {service, Result, RefNum};
        ?IPS_200_BG_A_IVRRELEASE_CNF ->
            <<RefNum:32/little, _:16/little, ReleaseResult:16/little>> = Command,
            {release, ReleaseResult, RefNum};
        ?IPS_200_BG_A_IPCONFCREATE_CNF ->
            <<ConfRefNum:32/little, ConfRoomID:16/little, AllocResult:16/little>> = Command,
            {create, AllocResult, {ConfRefNum, ConfRoomID}};
        ?IPS_200_BG_A_IPCONFCONTROL_CNF ->
            <<ConfRefNum:32/little, ConfRoomID:16/little, _:16, _:16/little, Result:16/little, _/binary>> = Command,
            {control, Result, {ConfRefNum, ConfRoomID}};
        ?IPS_200_BG_A_IPCONFDELETE_CNF->
            <<ConfRefNum:32/little, ConfRoomID:16/little, ReleaseResult:16/little>> = Command,
            {destory, ReleaseResult, {ConfRefNum, ConfRoomID}};
        _ ->
            none
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
gen_icp(Operation, Command) ->
    ICP_H = <<0:8              %% ProtocolType:8
            , 4:8              %% FrameType:8
            , 0:8              %% Sid:8
            , 0:8              %% Did:8
            , 0:8              %% SlotNum:8
            , 0:24/little>>,   %% BPH:24/little
    Frame = gen_frame(Operation, Command),
    Length = byte_size(Frame) + 8,
    <<Length:16/little
    , ICP_H/binary
    , Frame/binary>>.

gen_frame(Operation, Command) ->
    FRAME_H = <<0:32/little    %% AreaId:32/little
              , 0:32/little    %% Host:32/little
              , 0:32/little>>, %% Sender:32/little
    BodyLength = byte_size(Command),
    <<FRAME_H/binary
    , Operation:32/little
    , BodyLength:32/little
    , Command/binary>>.

gen_sub_command({Identifier, {Addr, Media, DC}}) when Identifier =:= 16#95->
    {{IP1, IP2, IP3, IP4}, Port} = Addr,
    {MediaAttribute, Payload} = Media,
    {DCType, DCPayload} = DC,
    _RtpConnectCmd = <<Identifier:8      %% Identifier:8
                     , 16#0C:8           %% Length:8
                     , IP4:8             %% ConnectionAddress:32/little
                     , IP3:8
                     , IP2:8
                     , IP1:8
                     , Port:16/little    %% MediaPort:16/little
                     , MediaAttribute:8  %% MediaAttribute:8
                     , Payload:8         %% PayloadType:8
                     , 20:8              %% PacketTime:8
                     , 0:8               %% ConnInd:8
                     , DCType:8          %% DigitCollectionType:8
                     , DCPayload:8>>;    %% PayloadType:8

gen_sub_command({Identifier, Param}) when Identifier =:= 16#b6->
    {FileName, Times, Interval, TotalTime} = Param,
    FilePosition = gen_sub_command({16#83, FileName}),
    Length = 4 + byte_size(FilePosition),
    _MutiMediaPlay = <<Identifier:8         %% Identifier:8
                     , Length:8             %% Length:8
                     , Times:8              %% PlayTimes:8
                     , Interval:8           %% InterPeriod:8
                     , TotalTime:16/little  %% TotalPeriod:16/little 0:nolimits
                     , FilePosition/binary>>;

gen_sub_command({Identifier, Param}) when Identifier =:= 16#83->
    Filename = list_to_binary(Param),
    Length = byte_size(Filename) + 15,
    _VoiceFile = <<Identifier:8        %% Identifier:8
                 , Length:8            %% Length:8
                 , 4:16/little         %% FileType:16/little
                 , 0:32/little         %% IP Address Of Server:32/little
                 , 0:16/little         %% Port Of Server:16/little
                 , 0:8                 %% ForceLoading:8
                 , 0:8                 %% Reserve:8
                 , 10:32/little        %% ReplaceAnnCode:32/little
                 , Filename/binary     %% Filename
                 , 0:8>>;

gen_sub_command({Identifier, _Param}) when Identifier =:= 16#fd->
    _PlayOffsetTime = <<Identifier:8   %% Identifier:8
                      , 2:16/little    %% Length:16/little
                      , 0:16/little>>; %% OffsetValue:16/little

gen_sub_command({Identifier, Param}) when Identifier =:= 16#b7->
    {Barge, MaxTime, FDT, IDT} = Param,
    _RtpDigitCollection = <<Identifier:8       %% Identifier:8
                          , 14:8               %% Length:8
                          , Barge:8            %% Brk:8 1:打断 0:不打断
                          , 0:8                %% Mincollect:8
                          , 128:8              %% Maxcollect:8
                          , MaxTime:8          %% Maxinteracttime:8(maxtime) 0:nolimits
                          , FDT:8              %% Initinterdgttime:8(fdt)
                          , IDT:8               %% Norminterdgttime:8(idt)
                          , 16#0800:16/little  %% Cleardgtmask(cancel=*)
                          , 16#1000:16/little  %% Enterdgtmask(rtk=#)
                          , 0:16/little        %% Startdgtmask1
                          , 0:16/little>>;     %% Startdgtmask2

gen_sub_command({Identifier, Param}) when Identifier =:= 16#b8->
    {MaxTime, FileName} = Param,
    VoiceFile = gen_sub_command({16#83, FileName}),
    FileRecordMode = gen_sub_command({16#fb}),
    Length = 14 + byte_size(VoiceFile) + byte_size(FileRecordMode),
    _RtpRecordParameter = <<Identifier:8        %% Identifier:8
                          , Length:8            %% Length:8
                          , MaxTime:16/little   %% RecordTime:16/little
                          , 0:8                 %% noVoiceTime:8(prespeech)
                          , 0:8                 %% SilenceTime:8(postspeech)
                          , 1:8                 %% BeepControl:8
                          , 2:8                 %% DtmfControl:8
                          , 16#0C01:16/little   %% RecordStopdgt:16/little(termkey=#)
                          , 0:16/little         %% RecordCanceldgt
                          , 0:16/little         %% RecordPlaydgt
                          , 0:16/little         %% RecordRestartdgt
                          , VoiceFile/binary
                          , FileRecordMode/binary>>;

gen_sub_command({Identifier}) when Identifier =:= 16#fb->
    _FileRecordMode = <<Identifier:8   %% Identifier:8
                      , 1:16/little    %% Length:16/little
                      , 0:8>>;         %% AppendValue:8

gen_sub_command({Identifier, RefNum}) when Identifier =:= 16#e9->
    _AssocResource = <<Identifier:8          %% Identifier:8
                     , 3:8                   %% Length:8
                     , 1:8                   %% AssocMid:8
                     , RefNum:16/little>>;   %% AssocResSlot:16/little

gen_sub_command({Identifier, {start, FileName}}) when Identifier =:= 16#7b->
    FilePathInfo = gen_sub_command({16#c1, FileName}),
    Length = 7 + byte_size(FilePathInfo),
    _IpConfRecPara = <<Identifier:8         %% Identifier:8
                     , Length:16/little     %% Length:16
                     , 1:8                  %% BCCmdType:8
                     , 1:8                  %% BCRecType:8
                     , 4:8                  %% BCRecFmt:8
                     , 0:16/little          %% RecTimeLen:16
                     , 0:16/little          %% RecFileSize:16
                     , FilePathInfo/binary>>;

gen_sub_command({Identifier, {stop, _}}) when Identifier =:= 16#7b->
    _IpConfRecPara = <<Identifier:8         %% Identifier:8
                     , 7:16/little          %% Length:16
                     , 3:8                  %% BCCmdType:8
                     , 1:8                  %% BCRecType:8
                     , 4:8                  %% BCRecFmt:8
                     , 0:16/little          %% RecTimeLen:16
                     , 0:16/little>>;       %% RecFileSize:16

gen_sub_command({Identifier, FileName}) when Identifier =:= 16#c1->
    Length = 6 + length(FileName),
    _FilePathInfo = <<Identifier:8          %% Identifier:8
                    , Length:16/little      %% Length:16
                    , 0:32/little           %% IP Address Of Server:32
                    , 0:16/little           %% Port of Server:16
                    , (list_to_binary(FileName))/binary>>;

gen_sub_command({Identifier, Param}) when Identifier =:= 16#7d->
    {ConfMembID, MembRefNum} = Param,
    _IpIvrResAddIn = <<Identifier:8          %% Identifier:8
                     , 11:16/little          %% Length:16/little
                     , MembRefNum:16/little  %% IVRSlot:16/little
                     , 1:8                   %% ResType:8
                     , ConfMembID:16/little  %% ConfMembID:16/little
                     , 2:8                   %% Type:8
                     , 16#03C2:16/little     %% AttributeFlag:16/little
                     , 16#0F:8               %% VolumeInGain:8
                     , 16#0F:8               %% VolumeOutGain:8
                     , 0:8>>;                %% PayloadFor2833:8

gen_sub_command({Identifier, MembRefNum}) when Identifier =:= 16#7f->
    _IpConfMemResID = <<Identifier:8             %% Identifier:8
                      , 2:8                      %% Length:8
                      , MembRefNum:16/little>>.  %% IVRSlot:16/little
