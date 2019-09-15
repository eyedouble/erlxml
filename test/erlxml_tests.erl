-module(erlxml_tests).

-include ( "../src/erlxml.hrl" ).
-include ( "test_helper.hrl" ).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

% main_test() ->
%     ?assertMatch(true, xml:main()).


get_element_data_type_test() ->
    {ok, SchemaState} = erlxml:build_schema_state({file, ?MOCK2XSD}),
    Element = #xmlText{} = {xmlText,[{'TotalPrice',68},{'Service',2},{'Services',72},{'GetBookingReply',2},{'Reply',1}], 1,[],"1900",text},
    Res = erlxml:get_element_data_type(Element, SchemaState),
    ?assertMatch(<<"xs:integer">>, Res).