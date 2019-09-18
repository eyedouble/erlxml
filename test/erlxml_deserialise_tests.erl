-module(erlxml_deserialise_tests).

-include ( "../src/erlxml.hrl" ).
-include ( "test_helper.hrl" ).
-include_lib("eunit/include/eunit.hrl").

deserialise_1_test() ->
    {ok, Schema} = erlxml:build_schema_state({file, ?MOCK1XSD}),
    Res = erlxml_deserialise:deserialise({file, ?MOCK1XML}, Schema),
    ?assertMatch(?MOCK1MAPRES, Res).

deserialise_2_test() ->
    {ok, Schema} = erlxml:build_schema_state({file, ?MOCK2XSD}),
    Res = erlxml_deserialise:deserialise({file, ?MOCK2XML}, Schema),
    ?assertMatch(?MOCK2MAPRES, Res).

deserialise_from_string_test() ->
    {ok, Xsd} = file:read_file(?MOCK2XSD),
    {ok, Schema} = erlxml:build_schema_state({binary, Xsd}),
    {ok, Xml} = file:read_file(?MOCK2XML),
    Res = erlxml_deserialise:deserialise(Xml, Schema),
    ?assertMatch(?MOCK2MAPRES, Res).

% deserialise_soap_test() ->
%     {ok, Schema} = erlxml:build_schema_state({file, ?MOCKBIDXSD}),
%     Res = erlxml_deserialise:deserialise({file, ?MOCKBID_RES_XML}, Schema),
%     ?assertMatch(?MOCK2MAPRES, Res).