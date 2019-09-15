-module(erlxml_serialise_tests).

-include ( "../src/erlxml.hrl" ).
-include ( "test_helper.hrl" ).
-include_lib("eunit/include/eunit.hrl").

serialise_test() ->
    {ok, Schema} = erlxml:build_schema_state({file, ?MOCK3XSD}),
    Res = erlxml_serialise:serialise(#{
        cereal_store => #{
            cereal => #{
                man => <<"Brinta Onbijtgranen">>,
                a => 100
            },                              
            <<"_xattributes">> => [{'EAN', "99 00 99 00"}]
        },
        <<"_xattributes">> => []
    }, Schema),
    ?assertMatch(true, Res).
