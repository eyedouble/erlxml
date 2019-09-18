%
% DESERIALISE MOCKS
%
-define(MOCK1XML, filename:absname("test/mock/test.xml")).
-define(MOCK1XSD, filename:absname("test/mock/test.xsd")).

-define(MOCK2XML, filename:absname("test/mock/reply.xml")).
-define(MOCK2XSD, filename:absname("test/mock/hostConnect.xsd")).

-define(MOCK3XML, filename:absname("test/mock/cereal.xml")).
-define(MOCK3XSD, filename:absname("test/mock/cereal.xsd")).

-define(MOCK1MAPRES, #{book_store :=
                       [#{book :=
                              #{author := <<"Joris Luyendijk">>,
                                date := <<"2006">>,
                                lala :=
                                    #{boe := <<"ta">>,foo := <<"Podium">>,
                                      qat := #{qty := <<"piel">>}},
                                price := 10,publisher := <<"Podium">>,
                                title := <<"Het zijn net mensen">>}},
                        #{book :=
                              #{author := <<"Joris Luyendijk">>,
                                date := <<"2006">>,
                                lala :=
                                    #{boe := <<"ta">>,foo := <<"Podium">>,
                                      qat := #{qty := <<"piel">>}}, 
                                price := 10,publisher := <<"Podium">>,
                                title := <<"Het zijn net mensen">>}},
                        #{book :=
                              #{author := <<"Joris Luyendijk">>,
                                date := <<"2006">>,
                                lala :=
                                    #{boe := <<"ta">>,foo := <<"Podium">>,
                                      qat := #{qty := <<"piel">>}},
                                price := 10,publisher := <<"Podium">>,
                                title := <<"Het zijn net mensen">>}}
                              ]}).


-define(MOCK2MAPRES, #{'Reply' :=
                       #{'GetBookingReply' :=
                             #{'TaxIndicator' := <<"0">>,
                               'CanAddServices' := <<"Y">>,
                               'SalesAnalysis6' := #{}, 
                               'TourplanConsultant' := <<"EA">>,
                               'BookingType' := <<"F">>,
                               'IsInternetBooking' := <<"Y">>,
                               'UDText3' := <<"99.99.99.99">>,
                               'ReadOnly' := <<"N">>,
                               'TravelDate' := <<"2018-11-16">>,
                               'BookingNotes' :=
                                   [#{'BookingNote' :=
                                          #{'NoteCategory' := <<"DOI">>,
                                            'NoteText' := <<"30921">>}},
                                    #{'BookingNote' :=
                                          #{'NoteCategory' := <<"FNM">>,
                                            'NoteText' := <<"John">>}},
                                    #{'BookingNote' :=
                                          #{'NoteCategory' := <<"PUI">>,
                                            'NoteText' := <<"30921">>}},
                                    #{'BookingNote' :=
                                          #{'NoteCategory' := <<"SNM">>,
                                            'NoteText' := <<"Doe">>}}],
                               'Remarks' := #{},'SalesAnalysis4' := #{},
                               'TourplanBookingStatus' := <<"PN">>,
                               'Consult' := #{},'AgentRef' := #{},
                               'Ref' := <<"DSCR356899">>,
                               'SalesAnalysis5' := <<"ZA">>,'Dialogue' := #{},
                               'Name' := <<"JOHN DOE">>,
                               'TotalPrice' := 198000,
                               'BookingUpdateCount' := <<"26">>,
                               'Email' := #{},'UDText4' := <<"nodata">>,
                               'SalesAnalysis3' := <<"DS">>,
                               'BookingId' := <<"272425">>,'QB' := <<"B">>,
                               'SalesAnalysis2' := <<"WS">>,
                               'UDText1' := <<"johndoe@gmail.com">>,
                               'UDText5' := #{},'BookingStatus' := #{},
                               'Currency' := <<"ZAR">>,
                               'UDText2' := <<"08112345678">>,
                               'SalesAnalysis1' := <<"ZA">>,
                               'EnteredDate' := <<"2018-11-11">>}}}).


%
%   SERIALISE MOCKS
%
-define(MOCK1MAP, #{
        cereal_store => #{
            cereal => #{
                man => <<"Brinta Onbijtgranen">>,
                name => <<"ZZZ">>,
                a => 100,
                b => true,
                c => {2019, 9, 6},
                d => 1.5
            },                              
            <<"_xattributes">> => [{'EAN', "99 00 99 00"}]
        },
        <<"_xattributes">> => []
    }).


-define(MOCK2MAP, #{'Reply' =>
                       #{'GetBookingReply' =>
                             #{'TaxIndicator' => <<"0">>,
                               'CanAddServices' => <<"Y">>,
                               'SalesAnalysis6' => #{}, 
                               'TourplanConsultant' => <<"EA">>,
                               'BookingType' => <<"F">>,
                               'IsInternetBooking' => <<"Y">>,
                               'UDText3' => <<"99.99.99.99">>,
                               'ReadOnly' => <<"N">>,
                               'TravelDate' => <<"2018-11-16">>,
                               'BookingNotes' => [
                                     #{'BookingNote' =>
                                          #{'NoteCategory' => <<"DOI">>,
                                            'NoteText' => <<"30921">>}},
                                    #{'BookingNote' =>
                                          #{'NoteCategory' => <<"FNM">>,
                                            'NoteText' => <<"John">>}},
                                    #{'BookingNote' =>
                                          #{'NoteCategory' => <<"PUI">>,
                                            'NoteText' => <<"30921">>}},
                                    #{'BookingNote' =>
                                          #{'NoteCategory' => <<"SNM">>,
                                            'NoteText' => <<"Doe">>}}
                                ],                                
                               'Remarks' => #{},'SalesAnalysis4' => #{},
                               'TourplanBookingStatus' => <<"PN">>,
                               'Consult' => #{},'AgentRef' => #{},
                               'Ref' => <<"DSCR356899">>,
                               'SalesAnalysis5' => <<"ZA">>,'Dialogue' => #{},
                               'Name' => <<"JOHN DOE">>,
                               'TotalPrice' => 198000,
                               'BookingUpdateCount' => <<"26">>,
                               'Email' => #{},'UDText4' => <<"nodata">>,
                               'SalesAnalysis3' => <<"DS">>,
                               'BookingId' => <<"272425">>,'QB' => <<"B">>,
                               'SalesAnalysis2' => <<"WS">>,
                               'UDText1' => <<"johndoe@gmail.com">>,
                               'UDText5' => #{},'BookingStatus' => #{},
                               'Currency' => <<"ZAR">>,
                               'UDText2' => <<"08112345678">>,
                               'SalesAnalysis1' => <<"ZA">>,
                               'EnteredDate' => <<"2018-11-11">>}}}).


%
%   BIDVEST SOAP MOCKS
%
-define(MOCKBID_REQ_XML, filename:absname("test/mock/bidvest.req.xml")).
-define(MOCKBID_RES_XML, filename:absname("test/mock/bidvest.res.xml")).
-define(MOCKBIDXSD, filename:absname("test/mock/bidvest.xsd")).