-module(erlxml_serialise).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlxml.hrl").

-export([
    serialise/2
]).

serialise(Struct, #erlXmlSchemaState{xsd_state=XsdState}=XS) ->
    ExpandedStruct = expand(Struct, [], XS),
    ?PRINT(ExpandedStruct),
    [StructForValidation] = xmerl_lib:expand_content(ExpandedStruct),  
    case xmerl_xsd:validate(StructForValidation, XsdState) of
        {error, Error} -> {error, Error};    
        {ValidatedXmlStruct,_Rest} -> 
            list_to_binary(xmerl:export([ValidatedXmlStruct], xmerl_xml))
    end.   



expand(#{}=Element, InternalState, #erlXmlSchemaState{xsd_state=XsdState}=XS) when map_size(Element) > 0 ->
    {Attrs, Element1} = case maps:is_key(<<"_xattributes">>, Element) of
        true -> maps:take(<<"_xattributes">>, Element);
        false -> {[], Element}
    end,
    
    Ordering = case maps:size(Element1) > 1 andalso proplists:is_defined(parent, InternalState) of
        true -> case erlxml:get_element_ordering(#xmlElement{name=proplists:get_value(parent, InternalState)}, XS) of
            {ok, Order} -> Order;
            {error, _} -> [] 
        end;
        false -> []
    end,
    List = case maps:to_list(Element1) of
        [{Tag, #{}=Val}] -> [{Tag, Attrs, expand(Val, [{parent, Tag}], XS)}];
        [{Tag, Val}] when is_list(Val) -> [{Tag, Attrs, [expand(E, InternalState, XS)||E<-Val]}];
        [{Tag, Val}] -> [{Tag, Attrs, [charlistize_content(Val)]}];
        Other -> expand(Other, InternalState, XS)   
    end,   
    sort_with_orderlist(List, Ordering, 1);
%% What about elements with attributes that have a list value?
%% PROPOSAL:
%%  adjust the map structure to allow values to be passed in like:
%%  #{ 'Tag' => #{ '_' => <<"Arb Value">>, <<"_xattrs">> => [{name, "val"}]}}
%%
expand([{Tag, [#{}=_|_]=Val}=_E|T], InternalState, XS) ->
    Attrs = [],
    [{Tag, Attrs, lists:foldl(fun(Element, Acc) ->
        Acc ++ expand(Element, InternalState, XS)
    end,[], Val)}] ++ expand(T, InternalState, XS);
  
expand([{_Tag, _Val}=E|T], InternalState, XS) ->
    expand(E, InternalState, XS) ++ expand(T, InternalState, XS);

expand({Tag, #{}=Val}=_E, _InternalState, _XS) when map_size(Val) =:= 0 ->
    [{Tag, [], []}];

expand({Tag, Val}=_E, _InternalState, _XS) ->
    [{Tag, [], [charlistize_content(Val)]}];

expand([], _InternalState, _XS) -> [].


%% @private
sort_with_orderlist(L, Keys, KeyPos) ->
   {Sorted, Tail} =
    lists:foldl(
       fun(K, {L1, Rest}) -> 
           {L1 ++ [X || X <- Rest, element(KeyPos,X) == K],
          [X || X <- Rest, element(KeyPos,X) =/= K]}
       end, {[], L}, Keys),
   Sorted ++ Tail.

%
%   Add float, bool, 
%
charlistize_content(X) when is_boolean(X) ->
    case X of
        true -> "true";
        false -> "false"
    end;
charlistize_content(X) when is_float(X) ->
    float_to_list(X, [{decimals, 20}, compact]);
charlistize_content(X) when is_integer(X) ->
    integer_to_list(X);
charlistize_content(X) when is_binary(X) ->
    binary_to_list(X);
charlistize_content({Y, M, D}) when
    is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
        ConPadY = fun
            (X) when X < 10 -> "000" ++ integer_to_list(X);
            (X) when X < 100 -> "00" ++ integer_to_list(X);
            (X) when X < 1000 -> "0" ++ integer_to_list(X);
            (X) -> integer_to_list(X)
        end,
        ConPadDM = fun
            (X) when X < 10 -> "0" ++ integer_to_list(X);
            (X) -> integer_to_list(X)
        end,
        ConPadY(Y) ++ "-" ++ ConPadDM(M) ++ "-" ++ ConPadDM(D).
