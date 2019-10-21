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