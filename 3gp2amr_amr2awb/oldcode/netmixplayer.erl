-module(netmixplayer).
-compile(export_all).
start() ->
    Files = ["audio1.wav","audio2.wav","audio3.wav"],
    register(command_processer, spawn(ocarina, command_processer, [])),
    register(ssrc, spawn(ocarina, ssrc, [1000])),
    Results = [myrpc:rpc(command_processer,{new, audio, {10,1,96,165}, Port_dest, Port_dest+1000})
                        || Port_dest <- lists:seq(5001,5003)],
    Players = [ Pid || {ok, Pid} <- Results],
    lists:map(fun(X) ->
                {Pid,File} = X,
                myrpc:rpc(command_processer, {load, Pid, File}) end,
              lists:zip(Players,Files)),
    [myrpc:rpc(command_processer, {play, Pid}) || Pid <- Players],
    ocarina:sleep(25000),
    over.

