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


get_element_ordering_test() ->
    {ok, SchemaState} = erlxml:build_schema_state({file, ?MOCK1XSD}),

    Element = #xmlElement{}={xmlElement,book,book,[],
          {xmlNamespace,[],[]},
          [{book_store,1}],
          1,[],
          [{xmlElement,title,title,[],
               {xmlNamespace,[],[]},
               [{book,1},{book_store,1}],
               1,[],
               [{xmlText,
                    [{title,1},{book,1},{book_store,1}],
                    1,[],"Title",text}],
               [],"/home/dag-development/Documents/erlang/erlxml",undeclared},
           {xmlElement,publisher,publisher,[],
               {xmlNamespace,[],[]},
               [{book,1},{book_store,1}],
               2,[],
               [{xmlText,
                    [{publisher,2},{book,1},{book_store,1}],
                    1,[],"Malmberg",text}],
               [],undefined,undeclared}],
          [],"/home/dag-development/Documents/erlang/erlxml",undeclared},

    Res = erlxml:get_element_ordering(Element, SchemaState),
 
    ?assertMatch({ok,[price,title,author,date,publisher,lala,'ISBN']}, Res).



get_element_ordering_advanced_test() ->
    {ok, SchemaState} = erlxml:build_schema_state({file, ?MOCK2XSD}),
    Element = #xmlElement{}={xmlElement,'GetBookingReply','GetBookingReply',[],
          {xmlNamespace,[],[]},
          [{'Reply',1}],
          1,[],
          [],
          [],"",undeclared},

    Res = erlxml:get_element_ordering(Element, SchemaState),
 
    ?assertMatch({ok,[
        'BookingId','Ref','BookingUpdateCount','Name','QB',
        'Consult','AgentRef','Email','TourplanBookingStatus',
        'UDText1','UDText2','UDText3','UDText4','UDText5',
        'TourplanConsultant','SalesAnalysis1','SalesAnalysis2',
        'SalesAnalysis3','SalesAnalysis4','SalesAnalysis5',
        'SalesAnalysis6','TaxIndicator','TravelDate',
        'EnteredDate','BookingStatus','ReadOnly',
        'CanAddServices','BookingType','IsInternetBooking',
        'Currency','TotalPrice','AgentPrice','Remarks',
        'Dialogue','BookingNotes','AccountingDetails',
        'Services'
    ]}, Res).



