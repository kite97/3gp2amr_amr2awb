-module(myrpc).
-compile(export_all).

rpc(Aim,Arg) ->
    %%io:format("RPC: ~p, Arg: ~p~n",[Aim, Arg]),
    if
        is_atom(Aim) -> Pid = whereis(Aim);
        is_pid(Aim) -> Pid = Aim
    end,
    Pid ! {self(), Arg},
    receive
        {Pid, Res} -> 
            %%io:format("RPC: from Pid: ~p Res is: ~p~n",[Pid, Res]),
            Res
    end.
