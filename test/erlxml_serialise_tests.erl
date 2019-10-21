-module(erlxml_serialise_tests).

-include ( "../src/erlxml.hrl" ).
-include ( "test_helper.hrl" ).
-include_lib("eunit/include/eunit.hrl").

serialise_test() ->
    {ok, Schema} = erlxml:build_schema_state({file, ?MOCK3XSD}),
    Res = erlxml_serialise:serialise(?MOCK1MAP, Schema),
    ?assertMatch({ok, <<"<?xml version=\"1.0\"?><cereal_store><cereal EAN=\"99 00 99 00\"><man>Brinta Onbijtgranen</man><name>ZZZ</name><a>100</a><b>true</b><c>2019-09-06</c><d>1.5</d></cereal></cereal_store>">>}, Res).

serialise2_test() ->
    {ok, Schema} = erlxml:build_schema_state({file, ?MOCK2XSD}),
    Res = erlxml_serialise:serialise(?MOCK2MAP, Schema),
    ?assertMatch({ok, <<"<?xml version=\"1.0\"?><Reply><GetBookingReply><BookingId>272425</BookingId><Ref>DSCR356899</Ref><BookingUpdateCount>26</BookingUpdateCount><Name>JOHN DOE</Name><QB>B</QB><Consult/><AgentRef/><Email/><TourplanBookingStatus>PN</TourplanBookingStatus><UDText1>johndoe@gmail.com</UDText1><UDText2>08112345678</UDText2><UDText3>99.99.99.99</UDText3><UDText4>nodata</UDText4><UDText5/><TourplanConsultant>EA</TourplanConsultant><SalesAnalysis1>ZA</SalesAnalysis1><SalesAnalysis2>WS</SalesAnalysis2><SalesAnalysis3>DS</SalesAnalysis3><SalesAnalysis4/><SalesAnalysis5>ZA</SalesAnalysis5><SalesAnalysis6/><TaxIndicator>0</TaxIndicator><TravelDate>2018-11-16</TravelDate><EnteredDate>2018-11-11</EnteredDate><BookingStatus/><ReadOnly>N</ReadOnly><CanAddServices>Y</CanAddServices><BookingType>F</BookingType><IsInternetBooking>Y</IsInternetBooking><Currency>ZAR</Currency><TotalPrice>198000</TotalPrice><Remarks/><Dialogue/><BookingNotes><BookingNote><NoteCategory>DOI</NoteCategory><NoteText>30921</NoteText></BookingNote><BookingNote><NoteCategory>FNM</NoteCategory><NoteText>John</NoteText></BookingNote><BookingNote><NoteCategory>PUI</NoteCategory><NoteText>30921</NoteText></BookingNote><BookingNote><NoteCategory>SNM</NoteCategory><NoteText>Doe</NoteText></BookingNote></BookingNotes></GetBookingReply></Reply>">>}, Res).
