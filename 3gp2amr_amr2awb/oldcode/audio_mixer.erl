-module(audio_mixer).
-compile(export_all).

-include("../include/wave.hrl").
-include("../include/rtp.hrl").

audio_mix(Audio1, Audio2, Length) when length(Audio1) >= Length, length(Audio2) >= Length ->
    Audio = lists:zip(lists:sublist(Audio1,Length),lists:sublist(Audio2,Length)),
    lists:map(fun({A1,A2}) ->
                      <<S1:16/signed>> = mulaw2pcm(<<A1:8>>),
                      <<S2:16/signed>> = mulaw2pcm(<<A2:8>>),
                      %%io:format("S1: ~p S2: ~p~n",[S1,S2]),
                      pcm2mulaw(<<((S1+S2) div 2):16/signed>>) end, Audio).

audio_mix(Audio1, Audio2) when length(Audio1) < length(Audio2) ->
    L = length(Audio1),
    audio_mix(Audio1, Audio2, L);
audio_mix(Audio1, Audio2) ->
    audio_mix(Audio2, Audio1).


audio_mix_test() ->
    audio_mix([1,3,5,7,9],[3,5,7,9,11],5).
mulaw2pcm(<<X:8/signed>>) ->
    BIAS = 16#84,
    CX = bnot X,
    <<S:1,SH:3,D:4>> = <<CX:8/signed>>,
    <<Data:16/signed>> = <<0:1,0:(7-SH),1:1,D:4,1:1,0:SH,0:2>>,
    if
        S == 0 -> <<(Data - BIAS):16/signed>>;
        S == 1 -> <<(BIAS - Data):16/signed>>
    end;

mulaw2pcm(X) ->
    mulaw2pcm(<<X:8/signed>>).

pcm2mulaw(<<X:16/signed>>) when X >= 0 ->
    L = (X + 16#84) band 16#7FFC,
    <<Data:8/signed>> = case <<L:16/signed>> of
        <<0:1,1:8,D:4,_:3>>  -> <<0:1,0:3,D:4>>;
        <<0:1,1:7,D:4,_:4>>  -> <<0:1,1:3,D:4>>;
        <<0:1,1:6,D:4,_:5>>  -> <<0:1,2:3,D:4>>;
        <<0:1,1:5,D:4,_:6>>  -> <<0:1,3:3,D:4>>;
        <<0:1,1:4,D:4,_:7>>  -> <<0:1,4:3,D:4>>;
        <<0:1,1:3,D:4,_:8>>  -> <<0:1,5:3,D:4>>;
        <<0:1,1:2,D:4,_:9>>  -> <<0:1,6:3,D:4>>;
        <<0:1,1:1,D:4,_:10>> -> <<0:1,7:3,D:4>>
    end,
    %%io:format("L: ~p Data: ~p~n",[L,Data]),
    <<(bnot Data):8/signed>>;

pcm2mulaw(<<X:16/signed>>) when X < 0 ->
    <<_:1,D:7>> = pcm2mulaw(-X),
    <<0:1,D:7>>;
pcm2mulaw(X) ->
    pcm2mulaw(<<X:16/signed>>).


audio_mixer(Audio, _Last) ->
    %%io:format("audio_mixer start~n"),
    receive
        {Pid, {get}} ->
            %%io:format("audio_mixer recv {get}~n"),
            [A1,A2] = lists:map(
                fun(A) ->
                    %%io:format("send A ~p {get}~n",[A]),
                    Data = myrpc:rpc(A,{get}),
                    %%io:format("get A ~p {get}~n",[A]),
                    [ X || <<X:8/signed>> <= Data] end,
                Audio),
            M = audio_mix(A1,A2),
            %%io:format("return m~n"),
            Pid ! {self(), M},
            audio_mixer(Audio,0)
    end.

sound_holder(Sound) ->
    receive
        {recv, Data} ->
            io:format("get sound piece~n"),
            {ok,Rtp} = rtp:decode(Data),
            Payload = Rtp#rtp.payload,
            sound_holder(Payload);
        {Pid, {get}} ->
            %%io:format("sound_holder recv {get}~n"),
            Pid ! {self(),Sound},
            sound_holder(Sound)
    after
        %%时间上不精确，暂时先不实现精确版
        20 ->
            %%io:format("~p set to zero~n",[self()]),
            sound_holder(<<(bnot 0):(160*8)/signed>>)
    end.


%%start() ->
%%    [Audio1|_] = (wave:read("audio1.wav"))#wave.data,
%%    [Audio2|_] = (wave:read("audio2.wav"))#wave.data,
%%    Mix = audio_mix(Audio1, Audio2, 159800),
%%    %%%%lists:max(Mix),
%%    %%Data = lists:map(fun(X) ->
%%    %%            mulaw2pcm(<<X:8>>) end, Audio1),
%%    %%Data2 = lists:map(fun(X) ->
%%    %%            pcm2mulaw(X) end, Data),
%%    %%io:format("~p~n ~p~n",[length((Data)), length(Audio1)]),
%%    file:write_file("mix.pcmu",list_to_binary(Mix), [binary]).
netmix() ->
    Holders = [spawn(audio_mixer, sound_holder, [<<(bnot 0):(160*8)/signed>>])
        || _ <- lists:seq(1,2)],
    %%[myrpc:rpc(Holder,{get}) || Holder <- Holders],
    %%[myrpc:rpc(Holder,{get}) || Holder <- Holders].
    L = length(Holders),
    Source = lists:zip(Holders,[X || X <- lists:seq(5001,5000+L)]),
    io:format("Source: ~p~n",[Source]),
    _Receivers = lists:map(fun({Holder,Port_src}) ->
        {ok, Socket} = gen_udp:open(Port_src,[{active,false},binary]),
        spawn(myudp, udp_receiver, [Socket, Holder]) end,
        Source),
    %%io:format("Receivers: ~p~n",[Receivers]),
    Pid = spawn(audio_mixer, audio_mixer,[Holders,0]),
    %%ocarina:sleep(100000).
    %%Data = myrpc:rpc(Pid, {get}),
    %%Data.
    Data = lists:map(fun(_) ->
                              ocarina:sleep(20),
                              %%io:format("Pid is: ~p~n",[Pid]),
                              myrpc:rpc(Pid,{get})
                      end,
                      lists:seq(1,1000)),
    %%length(list_to_binary(Data)).
    file:write_file("netmix.pcmu",list_to_binary(Data), [binary]).
