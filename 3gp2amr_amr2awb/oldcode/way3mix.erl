-module(way3mix).
-compile(export_all).
-include("../include/rtp.hrl").

loop(Arg) ->
    loop(Arg, 0).

loop({Socket, {Mixer, Target}}, Time) ->
    ocarina:sleep(17),
    %%io:format("Pid : ~p~n",[self()]),
    Data = myrpc:rpc(Mixer,{get}),
    Packet = rtp:encode(ocarina:genrtp(audio, Data, Time, 1000)),
    {Addr, Port} = Target,
    gen_udp:send(Socket, Addr, Port, Packet),
    loop({Socket, {Mixer,Target}}, Time + 160). 


start() ->
    Port_srcs = lists:seq(5001,5003),
    Targets = [ {{10,1,96,165}, Port} || Port <- lists:seq(60001,60003)],
    Holders = [spawn(audio_mixer, sound_holder, [<<(bnot 0):(160*8)/signed>>]) 
        || _ <- lists:seq(1,3)],
    Sockets = lists:map(fun(Port) ->
        {ok, Socket} = gen_udp:open(Port,[{active,false},binary]),
        Socket end,
        Port_srcs),
    Source = lists:zip(Holders,Sockets),
    lists:map(fun({Holder,Socket}) ->
        spawn(myudp, udp_receiver, [Socket, Holder]) end,
        Source),
    [Mixer1, Mixer2, Mixer3] = lists:map(fun({Holder1, Holder2}) ->
        _Pid = spawn(audio_mixer, audio_mixer,[[Holder1,Holder2],0]) end,
        [{H1, H2} || H1 <- Holders, H2 <- Holders,  H1 < H2]),
    Mixers = [Mixer1, Mixer2, Mixer3],
    [spawn(way3mix, loop, [Arg]) || Arg <- lists:zip(Sockets, lists:zip(Mixers, Targets))]. 
