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


%
% REFACTOR BELOW!!
%
deserialise_soap_test() ->
%     %
%     %   IMPLEMENTATION TO MOVE TO ACTUAL LIB!!!!!
%     %
    {ok, Schema} = erlxml:build_schemas_state({file, "mock"}),
    Res = erlxml_deserialise:deserialise({file, ?MOCKBID_RES_XML}, Schema),
    ?assertMatch(#{'MakeBookingWithQuoteResponse' :=
                       #{'MakeBookingWithQuoteResult' :=
                             #{'AccountID' := <<"RFCACC">>,
                               'CardAuth' := <<"1000">>,'Errors' := #{},
                               'RecordType' := <<"BR">>,
                               'RentalDuration' := <<"5">>,
                               'ReservationLines' :=
                                   #{'ReservationLine' :=
                                         #{'ResLineChrg' := <<"0.00">>,
                                           'ResLineChrgUnit' := #{},
                                           'ResLineCode' := <<"C">>,
                                           'ResLineDesc1' :=
                                               <<"Fuel - Vehicle will be refueled at Renters cost">>,
                                           'ResLineDesc2' := #{},
                                           'ResLineNil' := #{},
                                           'ResLineUnitChrg' := <<"0.00">>}},
                               'ReservationNumber' := <<"852852">>,
                               'VoucherNo' := <<"852858296">>}},
                   <<"_xattributes">> :=
                       [{xmlns,<<"http://www.budget.co.za">>}]}, Res).


serialise2_test() ->
    {ok, Schema} = erlxml:build_schemas_state({file, "mock"}),
    Res = erlxml_serialise:serialise(
                             #{'MakeBookingWithQuoteResponse' =>
                                   #{'MakeBookingWithQuoteResult' =>
                                         #{'AccountID' => <<"DISCAG">>,
                                           'CardAuth' => <<"4000">>,
                                           'Errors' => #{},
                                           'RecordType' => <<"BR">>,
                                           'RentalDuration' => 5,
                                           'ReservationLines' =>
                                               #{'ReservationLine' =>
                                                     #{'ResLineChrg' =>
                                                           <<"0.00">>,
                                                       'ResLineChrgUnit' =>
                                                           #{},
                                                       'ResLineCode' =>
                                                           <<"C">>,
                                                       'ResLineDesc1' =>
                                                           <<"Fuel - Vehicle will be refueled at Renters cost">>,
                                                       'ResLineDesc2' => #{},
                                                       'ResLineNil' => #{},
                                                       'ResLineUnitChrg' =>
                                                           <<"0.00">>}},
                                           'ReservationNumber' =>
                                               <<"4170577">>,
                                           'VoucherNo' => <<"15095327">>}},
                                <<"_xattributes">> =>
                                   [{xmlns,<<"http://www.budget.co.za">>}]
                    
                }, Schema),
    ?assertMatch({ok, <<"<?xml version=\"1.0\"?><MakeBookingWithQuoteResponse xmlns=\"http://www.budget.co.za\"><MakeBookingWithQuoteResult><AccountID>DISCAG</AccountID><RentalDuration>5</RentalDuration><RecordType>BR</RecordType><ReservationNumber>4170577</ReservationNumber><CardAuth>4000</CardAuth><VoucherNo>15095327</VoucherNo><ReservationLines><ReservationLine><ResLineCode>C</ResLineCode><ResLineDesc1>Fuel - Vehicle will be refueled at Renters cost</ResLineDesc1><ResLineUnitChrg>0.00</ResLineUnitChrg><ResLineChrgUnit/><ResLineDesc2/><ResLineChrg>0.00</ResLineChrg><ResLineNil/></ReservationLine></ReservationLines><Errors/></MakeBookingWithQuoteResult></MakeBookingWithQuoteResponse>">>}, Res).
