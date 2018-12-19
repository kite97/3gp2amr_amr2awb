-module(playmax).
-compile(export_all).

start() ->
    %%T = [wave:read("audio1.wav")|| _ <- lists:seq(1,200)],
    %%length(T).
    %%T = [file:read_file("audio1.wav") || _ <- lists:seq(1,1000)],
    %%length(T).
    %% Data = <<-1:(8*1024*300)/signed>>,
    %% _ = byte_size(Data),
    %% R = lists:map( fun(_) -> 
    %%                        %%[Wave] = wave:data2channels(Data, 1, 8),
    %%                        Wave = [ X || <<X:8/signed>> <= Data],
    %%                        Wave end,
    %%            lists:seq(1,200)),
    %% length(R).
    Data = lists:map(fun(_) -> 
                             [ 255 || _ <- lists:seq(1, 1024*300)] end,
                     lists:seq(1,300)),
    length(Data).
