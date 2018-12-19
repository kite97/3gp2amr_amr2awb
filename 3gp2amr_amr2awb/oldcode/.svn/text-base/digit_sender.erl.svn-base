-module(digit_sender).

-compile(export_all).

-include("../include/rtp.hrl").

start(Digits) ->
    {ok, Socket} = gen_udp:open(11111,[{active,false}, binary]),
    Packets = lists:map(fun(Digit) ->
                                Payload = rtp:encode_dtmf(#dtmf{event = Digit,
                                                                eof = true,
                                                                volume = 3,
                                                                duration = 160}),
                                _Packet = rtp:encode(#rtp{payload_type = 101,
                                                         sequence_number = 0,
                                                         timestamp = 0,
                                                         ssrc = 1024,
                                                         marker = 1,
                                                         payload = Payload})
                        end, Digits),
    Addr = {10,1,96,184},
    Port = 30000,
    lists:foreach(fun(Packet) ->
                          gen_udp:send(Socket, Addr, Port, Packet)
                  end, Packets),
    gen_udp:close(Socket),
    io:format("send over~n").
