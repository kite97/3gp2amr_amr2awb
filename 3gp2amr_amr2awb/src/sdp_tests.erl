-module(sdp_tests).

-include("sdp.hrl").
-include("../etc/test_sdp.config").

-define(TEMPFILE, "tmp_config.sdp").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_config_test_() ->
  {setup,
   fun start_parse_config/0,
   fun test_parse_config/1}.

parse_sdp_test_() ->
    {setup,
     fun start_parse_sdp/0,
     fun test_parse_sdp/1}.

negotiate_test_() ->
    {setup,
     fun start_negotiate/0,
     fun test_negotiate/1}.

generate_test_() ->
    {setup,
     fun start_generate/0,
     fun test_generate/1}.

start_parse_config() ->
    ?PARSE_CONFIG_TESTCASES.

test_parse_config(Testcases) ->
    lists:foldl(fun(I, T) -> gen_parse_config(I, T) end, [], Testcases).

start_parse_sdp() ->
    ?PARSE_SDP_TESTCASES.

test_parse_sdp(Testcases) ->
    lists:foldl(fun(I, T) -> gen_parse_sdp(I, T) end, [], Testcases).

start_negotiate() ->
    ?NEGO_TESTCASES.

test_negotiate(Testcases) ->
    lists:foldl(fun(I, R) -> gen_negotiate(I, R) end, [], Testcases).

start_generate() ->
    ?GEN_TESTCASES.

test_generate(Testcases) ->
    lists:foldl(fun(I, R) -> gen_generate(I, R) end, [], Testcases).

gen_parse_config({C, E}, T) ->
    generate_config(C),
    {_, R} = sdp:parse_sdp_config(?TEMPFILE),
    %%?debugFmt("parse_sdp_config result: ~p~nexpect: ~p~ncompare result: ~p~n~n", [R, E, R=:=E]),
    [?_assertEqual(E, R)|T].

generate_config(Content) ->
    {ok, F} = file:open(?TEMPFILE, [write]),
    lists:foreach(fun(I) -> io:format(F, "~w.~n", [I]) end, Content),
    file:close(F).

gen_parse_sdp({C, E}, T) ->
    {_, R} = sdp:parse_remote_sdp(C),
    %%?debugFmt("parse_remote_sdp result: ~p~nexpect: ~p~ncompare result: ~p~n~n", [R, E, R=:=E]),
    [?_assertEqual(E, R)|T].

gen_negotiate({{Remote, Init}, E}, T) ->
    {_, R} = sdp:negotiate(Remote, Init),
    %%?debugFmt("negotiate result: ~p~nexpect: ~p~ncompare result: ~p~n~n", [R, E, R=:=E]),
    [?_assertEqual(E, R)|T].

gen_generate({C, E}, T) ->
    {_, R} = sdp:generate(C),
    %%?debugFmt("generate result: ~p~nexpect: ~p~ncompare result: ~p~n~n", [R, E, R=:=E]),
    [?_assertEqual(E, R)|T].

-endif.
