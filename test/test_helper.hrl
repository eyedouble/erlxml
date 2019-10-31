%
% DESERIALISE MOCKS
%
-define(MOCK1XML, filename:absname("test/mock/test.xml")).
-define(MOCK1XSD, filename:absname("test/mock/test.xsd")).

-define(MOCK2XML, filename:absname("test/mock/reply.xml")).
-define(MOCK2XSD, filename:absname("test/mock/hostConnect.xsd")).

-define(MOCK3XML, filename:absname("test/mock/cereal.xml")).
-define(MOCK3XSD, filename:absname("test/mock/cereal.xsd")).

-define(MOCK4XML, filename:absname("test/mock/utf8_reply.xml")).
-define(MOCK4XSD, filename:absname("test/mock/hostConnect.xsd")).

-define(MOCK1MAPRES, #{book_store :=
                       [#{book :=
                              #{author := <<"Joris Luyendijk">>,
                                date := <<"2006">>,
                                lala :=
                                    #{boe := <<"ta">>,foo := <<"Podium">>,
                                      qat := #{qty := <<"mensen">>}},
                                price := 10,publisher := <<"Podium">>,
                                title := <<"Het zijn net mensen">>}},
                        #{book :=
                              #{author := <<"Joris Luyendijk">>,
                                date := <<"2006">>,
                                lala :=
                                    #{boe := <<"ta">>,foo := <<"Podium">>,
                                      qat := #{qty := <<"mensen">>}}, 
                                price := 10,publisher := <<"Podium">>,
                                title := <<"Het zijn net mensen">>}},
                        #{book :=
                              #{author := <<"Joris Luyendijk">>,
                                date := <<"2006">>,
                                lala :=
                                    #{boe := <<"ta">>,foo := <<"Podium">>,
                                      qat := #{qty := <<"mensen">>}},
                                price := 10,publisher := <<"Podium">>,
                                title := <<"Het zijn net mensen">>}}
                              ]}).


-define(MOCK2MAPRES, #{'Reply' :=
                       #{'GetBookingReply' :=
                             #{'TravelDate' := <<"2018-11-16">>,
                               'IsInternetBooking' := <<"Y">>,
                               'Email' := #{},
                               'ReadOnly' := <<"N">>,
                               'QB' := <<"B">>,
                               'BookingType' := <<"F">>,
                               'BookingUpdateCount' := <<"26">>,
                               'AgentRef' := #{},
                               'UDText5' := #{},
                               'SalesAnalysis2' := <<"WS">>,
                               'Ref' := <<"DSCR356899">>,
                               'SalesAnalysis3' := <<"DS">>,
                               'SalesAnalysis5' := <<"ZA">>,
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
                               'Remarks' := #{},'BookingId' := <<"272425">>,
                               'Name' := <<"JOHN DOE">>,
                               'BookingStatus' := #{},
                               'CanAddServices' := <<"Y">>,
                               'UDText4' := <<"nodata">>,
                               'TotalPrice' := 198000,
                               'Services' :=
                                   [#{'Service' :=
                                          #{'SupplierConfirmation' := #{},
                                            'Voucher_Status' := #{},
                                            'SequenceNumber' := <<"10">>,
                                            'Children' := <<"0">>,
                                            'ServiceCategory' := <<"S">>,
                                            'Opt' := <<"GENCRTEM001B_UUNL">>,
                                            'Date' := <<"2018-11-16">>,
                                            'Status' := <<"??">>,
                                            'SCU' := <<"day">>,
                                            'Infants' := <<"0">>,
                                            'Remarks' :=
                                                <<"Renter: john doe johndoe@gmail.com 08112345678 - Best time to call: Morning ZA - Nationality ZA - 99.99.99.99 Age 25 11 November 2018 @ 12:59 pm - Web Enquiry (Payment Possible) {payment_timestamp_placeholder} - Web Payment - {payment_amount_placeholder} DSA002 Tempest SA: B - Ford Figo Hatchback - Tempest Car Hire 11037546 GENCRTEM001B_UUNL Cape Town International Airport - 16 Nov 2018 @ 18:30 Cape Town International Airport - 22 Nov 2018 @ 18:30 R1980 (6 days x R330) Super Cover Unlimited km R3026 Excess R1500 Rental Security Deposit Extras Requested Contract Fee @ R85 Per Unit Estimated One Way Fee = R0 Total = R85 Border Crossing Requested Total = R0 Renter Comments: ------------------- ----- WP DATA ----- ------------------- [[veh_id:20107]] [[sup_id:9051]] [[tp_rate_id:2062788]] [[tp_option_code:GENCRTEM001B_SUNL]] [[owf_id:]] [[pickup_location:17191]] [[dropoff_location:17191]] [[pickup_date:16-11-2018]] [[pickup_time:18:30]] [[dropoff_date:22-11-2018]] [[dropoff_time:18:30]] [[country:South Africa]]">>,
                                            'CanUpdate' := <<"N">>,
                                            'Description' :=
                                                <<"Ford Figo Hatchback or Similar">>,
                                            'ServiceLineUpdateCount' :=
                                                <<"9">>,
                                            'LinePrice' := <<"198000">>,
                                            'CostedInBooking' := <<"Y">>,
                                            'TourplanServiceStatus' :=
                                                <<"IR">>,
                                            'Comment' := <<"Super Cover">>,
                                            'CanAccept' := <<"N">>,
                                            doTime := <<"1830">>,
                                            'SupplierName' :=
                                                <<"Tempest Car Hire 11037546">>,
                                            'LocationCode' := <<"GEN">>,
                                            'SCUqty' := <<"6">>,
                                            'Adults' := <<"1">>,
                                            'ServiceLineExpiryDate' :=
                                                <<"2018-11-16">>,
                                            'ServiceLineId' := <<"375246">>,
                                            puTime := <<"1830">>,
                                            'OptionNumber' := <<"2786">>,
                                            'ServiceLineNotes' :=
                                                [#{'ServiceLineNote' :=
                                                       #{'NoteCategory' :=
                                                             <<"SVN">>,
                                                         'NoteText' :=
                                                             <<"Extras Requested: Contract Fee @ R85 Per Unit">>}}],
                                            'Dropoff_Date' := <<"2018-11-22">>,
                                            'PriceCode' := <<"RR">>,
                                            'CancelDeleteStatus' := <<"C">>,
                                            puRemark :=
                                                <<"Cape Town International Airport">>,
                                            'Pickup_Date' := <<"2018-11-16">>,
                                            doRemark :=
                                                <<"Cape Town International Airport">>}}],
                               'SalesAnalysis1' := <<"ZA">>,
                               'TourplanConsultant' := <<"EA">>,
                               'SalesAnalysis4' := #{},
                               'EnteredDate' := <<"2018-11-11">>,
                               'UDText3' := <<"99.99.99.99">>,
                               'Consult' := #{},
                               'UDText1' := <<"johndoe@gmail.com">>,
                               'TaxIndicator' := <<"0">>,
                               'Currency' := <<"ZAR">>,
                               'TourplanBookingStatus' := <<"PN">>,
                               'SalesAnalysis6' := #{},
                               'UDText2' := <<"08112345678">>,
                               'Dialogue' := #{},
                               'AccountingDetails' :=
                                   #{'AmountDue' := <<"0">>,
                                     'CreditsTotal' := <<"0">>,
                                     'FutureBilling' := <<"198000">>,
                                     'InvoicedTotal' := <<"0">>,
                                     'ReceivedTotal' := <<"0">>}}}}).

-define(MOCK3MAPRES, #{'Reply' :=
                       #{'GetBookingReply' :=
                             #{'UDText2' := <<"07225874196">>,
                               'SalesAnalysis2' := <<"WS">>,
                               'AccountingDetails' :=
                                   #{'AmountDue' := <<"0">>,
                                     'CreditsTotal' := <<"0">>,
                                     'FutureBilling' := <<"1534000">>,
                                     'InvoicedTotal' := <<"0">>,
                                     'ReceivedTotal' := <<"0">>},
                               'QB' := <<"B">>,'BookingStatus' := #{},
                               'Currency' := <<"ZAR">>,
                               'TaxIndicator' := <<"0">>,
                               'TourplanBookingStatus' := <<"PN">>,
                               'UDText5' := #{},'SalesAnalysis3' := <<"DS">>,
                               'TravelDate' := <<"2019-12-14">>,
                               'SalesAnalysis1' := <<"ZA">>,
                               'SalesAnalysis6' := #{},
                               'Ref' := <<"AAAA401896">>,
                               'UDText1' := <<"john.doe@mail.com">>,
                               'AgentRef' := #{},'UDText4' := <<"nodata">>,
                               'EnteredDate' := <<"2019-10-23">>,
                               'Remarks' := #{},
                               'TourplanConsultant' := <<"GJ">>,
                               'ReadOnly' := <<"N">>,'Consult' := #{},
                               'SalesAnalysis4' := #{},
                               'BookingNotes' :=
                                   [#{'BookingNote' :=
                                          #{'NoteCategory' := <<"DOI">>,
                                            'NoteText' := <<"30850">>}},
                                    #{'BookingNote' :=
                                          #{'NoteCategory' := <<"FNM">>,
                                            'NoteText' := <<"JOHN">>}},
                                    #{'BookingNote' :=
                                          #{'NoteCategory' := <<"PUI">>,
                                            'NoteText' := <<"30850">>}},
                                    #{'BookingNote' :=
                                          #{'NoteCategory' := <<"SNM">>,
                                            'NoteText' := <<"DOE">>}}],
                               'Services' :=
                                   [#{'Service' :=
                                          #{'Children' := <<"0">>,
                                            doRemark := <<"Cape Town">>,
                                            'Pickup_Date' := <<"2019-12-14">>,
                                            puTime := <<"0700">>,
                                            'CostedInBooking' := <<"Y">>,
                                            'Description' :=
                                                <<"Hyundai H1 8 Seater or Similar">>,
                                            'CanUpdate' := <<"N">>,
                                            'TourplanServiceStatus' :=
                                                <<"IR">>,
                                            'SCU' := <<"day">>,
                                            'Comment' := <<"Super Cover">>,
                                            doTime := <<"0700">>,
                                            'SupplierConfirmation' := #{},
                                            'SequenceNumber' := <<"10">>,
                                            'SCUqty' := <<"13">>,
                                            'Voucher_Status' := #{},
                                            'LinePrice' := <<"1534000">>,
                                            'ServiceLineUpdateCount' :=
                                                <<"10">>,
                                            'ServiceLineExpiryDate' :=
                                                <<"2019-12-14">>,
                                            'ServiceLineNotes' :=
                                                [#{'ServiceLineNote' :=
                                                       #{'NoteCategory' :=
                                                             <<"SVN">>,
                                                         'NoteText' :=
                                                             <<"Extras Requested: Contract Fee @ R90 Per Unit Border Crossing Requested Namibia Total = R1350">>}}],
                                            'ServiceCategory' := <<"S">>,
                                            'Remarks' :=
                                                <<82,101,110,116,101,114,58,32,
                                                  74,111,104,110,32,68,111,101,
                                                  32,106,111,104,110,46,100,
                                                  111,101,64,109,97,105,108,46,
                                                  99,111,109,32,48,55,50,56,57,
                                                  53,55,52,53,56,53,32,90,65,
                                                  32,45,32,78,97,116,105,111,
                                                  110,97,108,105,116,121,32,90,
                                                  65,32,45,32,49,48,48,46,49,
                                                  48,48,46,49,53,48,46,51,53,
                                                  32,65,103,101,32,51,53,32,50,
                                                  51,32,79,99,116,111,98,101,
                                                  114,32,50,48,49,57,32,64,32,
                                                  54,58,48,52,32,112,109,32,45,
                                                  32,87,101,98,32,69,110,113,
                                                  117,105,114,121,32,40,80,97,
                                                  121,109,101,110,116,32,78,
                                                  111,116,32,80,111,115,115,
                                                  105,98,108,101,41,32,123,112,
                                                  97,121,109,101,110,116,95,
                                                  116,105,109,101,115,116,97,
                                                  109,112,95,112,108,97,99,101,
                                                  104,111,108,100,101,114,125,
                                                  32,45,32,87,101,98,32,80,97,
                                                  121,109,101,110,116,32,45,32,
                                                  123,112,97,121,109,101,110,
                                                  116,95,97,109,111,117,110,
                                                  116,95,112,108,97,99,101,104,
                                                  111,108,100,101,114,125,32,
                                                  65,65,65,48,48,49,32,84,101,
                                                  109,112,101,115,116,32,83,65,
                                                  58,32,69,32,45,32,72,121,117,
                                                  110,100,97,105,32,72,49,32,
                                                  56,32,83,101,97,116,101,114,
                                                  32,45,32,84,101,109,112,101,
                                                  115,116,32,67,97,114,32,72,
                                                  105,114,101,32,49,49,48,51,
                                                  55,53,52,54,32,71,69,78,67,
                                                  82,84,69,77,48,48,49,69,95,
                                                  85,85,78,76,32,67,97,112,101,
                                                  32,84,111,119,110,32,45,32,
                                                  49,52,32,68,101,99,32,50,48,
                                                  49,57,32,64,32,48,55,58,48,
                                                  48,32,67,97,112,101,32,84,
                                                  111,119,110,32,45,32,50,55,
                                                  32,68,101,99,32,50,48,49,57,
                                                  32,64,32,48,55,58,48,48,32,
                                                  82,49,53,51,52,48,32,40,49,
                                                  51,32,100,97,121,115,32,120,
                                                  32,82,49,49,56,48,41,32,83,
                                                  117,112,101,114,32,67,111,
                                                  118,101,114,32,85,110,108,
                                                  105,109,105,116,101,100,32,
                                                  107,109,32,82,53,48,52,52,32,
                                                  69,120,99,101,115,115,32,82,
                                                  49,53,48,48,32,82,101,110,
                                                  116,97,108,32,83,101,99,117,
                                                  114,105,116,121,32,68,101,
                                                  112,111,115,105,116,32,69,
                                                  120,116,114,97,115,32,82,101,
                                                  113,117,101,115,116,101,100,
                                                  32,67,111,110,116,114,97,99,
                                                  116,32,70,101,101,32,64,32,
                                                  82,57,48,32,80,101,114,32,85,
                                                  110,105,116,32,69,115,116,
                                                  105,109,97,116,101,100,32,79,
                                                  110,101,32,87,97,121,32,70,
                                                  101,101,32,61,32,82,32,48,32,
                                                  84,111,116,97,108,32,61,32,
                                                  82,57,48,32,66,111,114,100,
                                                  101,114,32,67,114,111,115,
                                                  115,105,110,103,32,82,101,
                                                  113,117,101,115,116,101,100,
                                                  32,78,97,109,105,98,105,97,
                                                  32,84,111,116,97,108,32,61,
                                                  32,82,49,51,53,48,32,82,101,
                                                  110,116,101,114,32,67,111,
                                                  109,109,101,110,116,115,58,
                                                  32,68,111,101,115,32,116,104,
                                                  101,32,104,121,117,110,100,
                                                  97,105,32,72,49,32,99,111,
                                                  109,101,32,105,110,32,97,117,
                                                  116,111,109,97,116,105,99,46,
                                                  32,73,226,128,153,109,32,115,
                                                  111,114,114,121,32,116,104,
                                                  101,32,112,114,101,118,105,
                                                  111,117,115,32,101,110,113,
                                                  117,105,114,101,115,32,108,
                                                  111,99,97,116,105,111,110,32,
                                                  119,97,115,32,119,114,111,
                                                  110,103,32,45,45,45,45,45,45,
                                                  45,45,45,45,45,45,45,45,45,
                                                  45,45,45,45,32,45,45,45,45,
                                                  45,32,87,80,32,68,65,84,65,
                                                  32,45,45,45,45,45,32,45,45,
                                                  45,45,45,45,45,45,45,45,45,
                                                  45,45,45,45,45,45,45,45,32,
                                                  91,91,118,101,104,95,105,100,
                                                  58,50,48,49,49,48,93,93,32,
                                                  91,91,115,117,112,95,105,100,
                                                  58,57,48,53,49,93,93,32,91,
                                                  91,116,112,95,114,97,116,101,
                                                  95,105,100,58,51,49,51,50,51,
                                                  57,55,93,93,32,91,91,116,112,
                                                  95,111,112,116,105,111,110,
                                                  95,99,111,100,101,58,71,69,
                                                  78,67,82,84,69,77,48,48,49,
                                                  69,95,83,85,78,76,93,93,32,
                                                  91,91,111,119,102,95,105,100,
                                                  58,93,93,32,91,91,112,105,99,
                                                  107,117,112,95,108,111,99,97,
                                                  116,105,111,110,58,49,55,51,
                                                  48,56,93,93,32,91,91,100,114,
                                                  111,112,111,102,102,95,108,
                                                  111,99,97,116,105,111,110,58,
                                                  49,55,51,48,56,93,93,32,91,
                                                  91,112,105,99,107,117,112,95,
                                                  100,97,116,101,58,49,52,45,
                                                  49,50,45,50,48,49,57,93,93,
                                                  32,91,91,112,105,99,107,117,
                                                  112,95,116,105,109,101,58,48,
                                                  55,58,48,48,93,93,32,91,91,
                                                  100,114,111,112,111,102,102,
                                                  95,100,97,116,101,58,50,55,
                                                  45,49,50,45,50,48,49,57,93,
                                                  93,32,91,91,100,114,111,112,
                                                  111,102,102,95,116,105,109,
                                                  101,58,48,55,58,48,48,93,93,
                                                  32,91,91,99,111,117,110,116,
                                                  114,121,58,83,111,117,116,
                                                  104,32,65,102,114,105,99,97,
                                                  93,93,32,91,91,99,97,114,95,
                                                  116,121,112,101,58,49,93,93,
                                                  32>>,
                                            'LocationCode' := <<"GEN">>,
                                            'ServiceLineId' := <<"437372">>,
                                            'CanAccept' := <<"N">>,
                                            'OptionNumber' := <<"2796">>,
                                            'Status' := <<"??">>,
                                            'Infants' := <<"0">>,
                                            'Adults' := <<"1">>,
                                            'Date' := <<"2019-12-14">>,
                                            puRemark := <<"Cape Town">>,
                                            'CancelDeleteStatus' := <<"C">>,
                                            'Opt' := <<"GENCRTEM001E_UUNL">>,
                                            'PriceCode' := <<"RR">>,
                                            'Dropoff_Date' := <<"2019-12-27">>,
                                            'SupplierName' :=
                                                <<"Tempest Car Hire 11037546">>}}],
                               'BookingType' := <<"F">>,
                               'BookingUpdateCount' := <<"28">>,
                               'TotalPrice' := 1534000,
                               'IsInternetBooking' := <<"Y">>,
                               'UDText3' := <<"100.100.150.35">>,
                               'BookingId' := <<"318421">>,
                               'CanAddServices' := <<"Y">>,'Email' := #{},
                               'Dialogue' := #{},'Name' := <<"JOHN DOE">>,
                               'SalesAnalysis5' := <<"ZA">>}}}).

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