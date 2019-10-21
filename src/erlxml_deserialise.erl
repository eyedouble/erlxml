-module(erlxml_deserialise).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlxml.hrl").

-export([
    deserialise/2
]).

-define(WITH_ATTRS, true).
deserialise({file, XmlPath}, #erlXmlSchemaState{xsd_state=_XsdState}=XS) -> 
   {StructForValidation,_} = xmerl_scan:file(XmlPath, [{space, normalize}]),
   deserialise({xmerl_structure, StructForValidation}, XS);

deserialise(<<Xml/binary>>, #erlXmlSchemaState{xsd_state=_XsdState}=XS) ->
    {StructForValidation,_} = xmerl_scan:string(binary_to_list(Xml), [{space, normalize}]),
    deserialise({xmerl_structure, StructForValidation}, XS);

deserialise({xmerl_structure, StructForValidation}, #erlXmlSchemaState{xsd_state=XsdState}=XS) ->
    case xmerl_xsd:validate(StructForValidation, XsdState) of
        {error, Error} -> {error, Error};    
        {Struct,_Rest} ->
            CleanStruct = xmerl_lib:remove_whitespace([Struct]),
            simplify(CleanStruct, XS)
    end.

simplify([#xmlElement{name = Name, attributes = _Attrs, content = Content}=Element|T], #erlXmlSchemaState{}=XS)->
    case erlxml:is_list_element(Element, XS) of
        true ->
            TransElement = #{Name => [simplify(E, XS)|| E <- Content]},
            case simplify(T, XS) of
                #{} -> TransElement;
                Other -> [TransElement, Other]
            end;
        false ->
            maps:merge(simplify(Element, XS), simplify(T, XS))
    end;
simplify([Car|Cdr], #erlXmlSchemaState{}=XS) ->
    case [simplify(Car, XS), simplify(Cdr, XS)] of
        [Car1|Cdr1] when Cdr1 =:= [#{}] -> Car1;
        [_Car1|_Cdr1]=Pass -> Pass
    end;    
simplify(#xmlElement{name = Name, attributes = Attrs, content = Content}=_Element, #erlXmlSchemaState{}=XS) ->
    MapElement = #{Name => simplify(Content, XS)},
    case Attrs =/= [] andalso ?WITH_ATTRS =:= true of
        true -> maps:merge(MapElement, #{<<"_xattributes">> => simplify_attributes(Attrs)});
        false -> MapElement
    end;
simplify(#xmlText{value=Value}=Val, #erlXmlSchemaState{}=XS) ->
    case erlxml:get_element_data_type(Val, XS) of
        <<"xs:decimal">> -> list_to_float(Value);
        <<"xs:float">> -> list_to_float(Value);
        <<"xs:int">> -> list_to_integer(Value);
        <<"xs:integer">> -> list_to_integer(Value);
        <<"xs:negativeInteger">> -> list_to_integer(Value);
        <<"xs:nonNegativeInteger">> -> list_to_integer(Value);
        <<"xs:nonPositiveInteger">> -> list_to_integer(Value);
        <<"xs:date">> -> conv_func_date(Value);
        <<"xs:boolean">> -> conv_func_boolean(Value);
        _Other -> list_to_binary(Value)
    end;
simplify([], #erlXmlSchemaState{}=_XS) -> #{}.

%%
%%  Conversion functions
%%
conv_func_date(X) ->
    case list_to_binary(X) of
        <<Y:4, "-", M:2, "-", D:2>> -> {Y, M, D};
        AllBinary -> AllBinary
    end.
conv_func_boolean(X) ->
    case list_to_binary(X) of
        <<"true">> -> true;
        <<"false">> -> false;
        <<"0">> -> 0;
        <<"1">> -> 1;
        AllBinary -> AllBinary
    end.


%%
%% Attributes
%%
simplify_attributes([#xmlAttribute{name = K, value = V} | T])
  when is_atom(K) ->
    [{K, expand_value(V)} | simplify_attributes(T)];
simplify_attributes([H = {K, _} | T]) when is_atom(K) ->
    [H | simplify_attributes(T)];
simplify_attributes([]) ->
    [].

expand_value(S) when is_atom(S) ->
    atom_to_binary(S, utf8);
expand_value(S) when is_integer(S) ->
    integer_to_binary(S);
expand_value(S) ->
    list_to_binary(S).
