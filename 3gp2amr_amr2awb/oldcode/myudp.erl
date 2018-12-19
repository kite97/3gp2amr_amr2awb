-module(myudp).
-compile(export_all).
udp_sender(Socket, Address, Port) ->
    receive
        {Data} ->
            gen_udp:send(Socket, Address, Port, Data),
            udp_sender(Socket, Address, Port);
        stop ->
            stop;
        _ ->
            udp_sender(Socket, Address, Port)
    end.

udp_receiver(Socket, Pid) ->
    {ok,{_, _, Data}} = gen_udp:recv(Socket, 65536),
    io:format("udp get data, send to ~p~n",[Pid]),
    Pid ! {recv, Data},
    udp_receiver(Socket,Pid).
