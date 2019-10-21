-module(erlxml_serialise).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlxml.hrl").

-export([
    serialise/2
    ,serialise/3
    ,serialise/4
    ,rescan_for_namespacing/2
]).

-define(TIMEOUT, 175).

serialise(Struct, #erlXmlSchemaState{xsd_state=_XsdState}=XS) -> serialise(Struct, XS, "<?xml version=\"1.0\"?>").

serialise(Struct, #erlXmlSchemaState{xsd_state=_XsdState}=XS, Prolog) ->
    Ref = make_ref(),
    Pid = self(),
    spawn_link(?MODULE, serialise, [{Pid, Ref}, Struct, XS, Prolog]),
    receive
        {serialise, {Pid, Ref}, Res} -> Res
    after
        ?TIMEOUT -> {error, <<"Serialisation process exceeded timeout limit">>}
    end.
            
%% @private
serialise({Pid, Ref}, Struct, #erlXmlSchemaState{xsd_state=_XsdState}=XS, Prolog) when is_pid(Pid) andalso is_reference(Ref) ->
    ExpandedStruct = expand(Struct, [], XS),
    [StructForValidation] = erlxml_lib:expand_content(ExpandedStruct),  

    Ref1 = make_ref(),
    Pid1 = self(),
    spawn_link(?MODULE, rescan_for_namespacing, [{Pid1, Ref1}, StructForValidation]),   

    Res = case validate(StructForValidation, XS) of
        {error, _Error} ->   
            receive 
                {rescan_namespacing, {Pid1, Ref1}, StructureWithNamespacing} -> 
                    case validate(StructureWithNamespacing, XS) of
                        {error, Error} -> {error, Error};
                        {ok, ValidatedNamespacedXmlStructure} ->
                            {ok, list_to_binary(xmerl:export([ValidatedNamespacedXmlStructure], xmerl_xml, []))}
                    end
            after 
                (?TIMEOUT - 10) -> {error, <<"Namespace rebuild process exceeded timeout limit">>}
            end;
        {ok, ValidatedXmlStruct} ->            
            {ok, list_to_binary(xmerl:export([ValidatedXmlStruct], xmerl_xml, [#xmlAttribute{name=prolog, value=Prolog}]))}
    end,
    Pid ! {serialise, {Pid, Ref}, Res}. 

%% @private
validate(StructForValidation, #erlXmlSchemaState{xsd_state=XsdState}=_XS) -> 
    case xmerl_xsd:validate(StructForValidation, XsdState) of
        {error, Error} -> {error, Error};   
        {ValidatedXmlStruct,_Rest} -> {ok, ValidatedXmlStruct}
    end.

%% @private
rescan_for_namespacing({From, Ref}, Struct) when is_pid(From) andalso is_reference(Ref) ->
    Q = xmerl:export([Struct], xmerl_xml),
    {StructureWithNamespacing, _} = xmerl_scan:string(lists:flatten(Q), [{space, normalize}]),
    From ! {rescan_namespacing, {From, Ref}, StructureWithNamespacing}.

%%
%%  Expansion
%%
%% @private
expand(#{}=Element, InternalState, #erlXmlSchemaState{xsd_state=_XsdState}=XS) when map_size(Element) > 0 ->
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
%% @private
expand([{Tag, [#{}=_|_]=Val}=_E|T], InternalState, XS) ->
    Attrs = [],
    [{Tag, Attrs, lists:foldl(fun(Element, Acc) ->
        Acc ++ expand(Element, InternalState, XS)
    end,[], Val)}] ++ expand(T, InternalState, XS);

expand([{_Tag, _Val}=E|T], InternalState, XS) ->
    expand(E, InternalState, XS) ++ expand(T, InternalState, XS);

expand({Tag, #{}=Val}=_E, _InternalState, _XS) when map_size(Val) =:= 0 ->
    [{Tag, [], []}];

expand({Tag, Val}=_E, _InternalState, _XS) when is_map(Val) =:= false ->
    [{Tag, [], [charlistize_content(Val)]}];

expand({Tag, Val}=_E, InternalState, XS) ->
    [{Tag, [], lists:flatten([expand(Val, InternalState, XS)])}];

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

%% @private
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
