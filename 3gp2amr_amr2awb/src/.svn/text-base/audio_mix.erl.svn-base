-module(audio_mix).

-export([mix_8khz/1, mix_16khz/1]).

-on_load(init/0).

init() ->
    erlang:load_nif("../priv/mixer", 0).

mix_8khz(_) ->
    "mix_8khz load fail".

mix_16khz(_) ->
    "mix_16khz load fail".
