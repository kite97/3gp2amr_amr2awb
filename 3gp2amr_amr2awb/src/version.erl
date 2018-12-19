-module(version).
-export([get_version/0]).
-define(VERSION,"3.1.0").

get_version()->
    ?VERSION.
