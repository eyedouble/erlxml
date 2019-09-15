-module(erlxml_serialise).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlxml.hrl").

-export([
    serialise/2
]).

serialise(Struct, #erlXmlSchemaState{xsd_state=XsdState}=XS) ->
    {S, R} = xmerl_scan:string("<book_store><book><title>Title</title><publisher>Malmberg</publisher></book></book_store>"),
    % T = xmerl_lib:simplify_content([S]),
    ?PRINT(S),
    ExpandedStruct = expand(Struct),
    [StructForValidation] = xmerl_lib:expand_content(ExpandedStruct),
    ?PRINT(StructForValidation),
   
    case xmerl_xsd:validate(StructForValidation, XsdState) of
        {error, Error} -> {error, Error};    
        {Struct,_Rest} -> {ok, Struct}
    end.


%
% BUG WITH :
%
% cereal_store => #{
    %     cereal => #{
    %         name => <<"Brinta Onbijtgranen">>      <- HERE ONE ELEMENT ONLY!       
    %     }
    % },
%
expand(#{}=Element) ->
    {Attrs, Element1} = case maps:is_key(<<"_xattributes">>, Element) of
        true -> maps:take(<<"_xattributes">>, Element);
        false -> {[], Element}
    end,
    case maps:to_list(Element1) of
        [{Tag, #{}=Val}] -> [{Tag, Attrs, expand(Val)}];
        [{Tag, Val}] when is_list(Val) -> [{Tag, Attrs, [expand(E)||E<-Val]}];
        [{Tag, Val}] -> [{Tag, Attrs, charlistize_content(Val)}];
        Other -> expand(Other)   
    end;
expand([{Tag, Val}=E|T]) ->
    expand(E) ++ expand(T);
expand({Tag, Val}=E) ->
        [{Tag, [], [charlistize_content(Val)]}];
expand([]) -> [].

charlistize_content(X) when is_integer(X) ->
    integer_to_list(X);
charlistize_content(X) when is_binary(X) ->
    binary_to_list(X).

