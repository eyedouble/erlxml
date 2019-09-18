-module(erlxml).

-include_lib("xmerl/include/xmerl.hrl").
-include("erlxml.hrl").

-define(TMPFILE, ".tmp.xsd").

-export([
    main/0
    ,build_schema_state/1
    ,get_element_data_type/2
    ,is_list_element/2
    ,get_element_ordering/2
]).

build_schema_state({binary, <<RawSchema/binary>>}) ->
    file:write_file(?TMPFILE, RawSchema),
    case filelib:is_file(?TMPFILE) of 
        true -> build_schema_state({file, filename:absname(?TMPFILE)});
        false -> {error, file:delete(?TMPFILE), <<"Could not write tmp schema file">>}
    end;
build_schema_state({file, SchemaPath}) ->
    {Schema,_Rest} = xmerl_scan:file(SchemaPath, []),
    {ok, Model} = xmerl_xsd:process_schema(SchemaPath),
    {ok, #erlXmlSchemaState{struct=Schema, xsd_state=Model}}.


is_list_element(#xmlElement{name=Name}=Element, #erlXmlSchemaState{struct=SchemaStruct}=_XS) ->
    case [E|| #xmlElement{}=E <- xmerl_xpath:string("//xs:element[@name='" ++ atom_to_list(Name) ++ "']//*[@maxOccurs]", SchemaStruct)] of
        [#xmlElement{attributes=Attrs}=_Car|_Cdr] -> [true|| #xmlAttribute{name=AttrName, value=AttrVal} <- Attrs, AttrName =:= maxOccurs, AttrVal =/= "1"] =/= [];
        [] -> false
    end.

get_element_data_type(#xmlText{parents=Parents}=Element,#erlXmlSchemaState{struct=SchemaStruct}=_XS) ->
    case Parents of
        [{Name, _Pos}=_DirectParent|_Rest] when is_atom(Name) ->
            case xmerl_xpath:string("//xs:element[@name='" ++ atom_to_list(Name) ++ "']/@type", SchemaStruct) of
                [#xmlAttribute{value=AttrValue}] -> 
                    list_to_binary(AttrValue);
                _Other1 -> {ok, <<"xs:string">>}
            end;
        _Other -> {ok, <<"xs:string">>}
    end.

get_element_ordering(#xmlElement{name=Name}=Element, #erlXmlSchemaState{struct=SchemaStruct}=XS) ->    
    case [Content || #xmlElement{content=Content} <- xmerl_xpath:string("//xs:element[@name='" ++ atom_to_list(Name) ++ "']", SchemaStruct), Content =/= []] of
        [Content|_] -> determine_order(Content);
        [] -> case get_referenced_element_content(Element, XS) of
            [_|_]=Content -> determine_order(Content)
        end
    end.

%% @private
get_referenced_element_content(#xmlElement{name=Name}=Element, #erlXmlSchemaState{struct=SchemaStruct}=XS) -> 
    FindElementsThatReference = fun(FName, FSchemaStruct) -> 
        [Attr||
            #xmlElement{attributes=Attrs}=_DefElement <- xmerl_xpath:string("//xs:element[@name='" ++ atom_to_list(FName) ++ "'][@type or @ref]", FSchemaStruct),
            #xmlAttribute{name=AttrName}=Attr <- Attrs, (AttrName =:= type orelse AttrName =:= ref)]
    end,

    FindReferencedElementContent = fun(REName, RESchemaStruct) ->
        case xmerl_xpath:string("//*[@name='" ++ REName ++ "']", RESchemaStruct) of
            [#xmlElement{content=Content}=_E] -> Content;
            _Other -> {error, <<"Could not determine order from element selected in order definition">>}
        end
    end,

    case FindElementsThatReference(Name, SchemaStruct) of
        [#xmlAttribute{name=_Name, value=Value}=_Attr|_] -> FindReferencedElementContent(Value, SchemaStruct);     
        _Other -> {error, <<"Could not find suitable element for order definition">>}
    end.

%% @private
determine_order(Content) ->
    {ok, lists:flatten(recursive_order_builder(Content))}.


%% @private
recursive_order_builder([Car|Cdr]) ->
    [recursive_order_builder(Car), recursive_order_builder(Cdr)];
recursive_order_builder(#xmlElement{attributes=Attrs, content=Content}) ->
    P = [list_to_atom(Value)||#xmlAttribute{name=Name, value=Value} <- Attrs, (Name =:= name orelse Name =:= ref)],
    [P, recursive_order_builder(Content)];
recursive_order_builder(#xmlText{}=_) -> [];
recursive_order_builder([]) -> [].





% find_element_in_schema(#xmlElement{name=Name}=Element) ->
%     {Xsd,_Rest} = xmerl_scan:file("test.xsd", []),
%     [ElementSchema|_] = [E|| #xmlElement{}=E <- ],
%     ElementSchema.

make_typemap() ->
    {Xsd,_Rest} = xmerl_scan:file("test.xsd", []),
    RawTypeList = lists:foldl(fun(Type, Acc) ->
       Acc ++ [ {list_to_binary(Name), Type} || {xmlAttribute,name,_,_,_,_Hierarchy,_,[],Name,_} <- xmerl_xpath:string("//xsd:element[@name][@type='" ++ Type ++ "']/@name", Xsd)] 
    end, [], ["xsd:integer", "xsd:date"]),
    TypeMap = maps:from_list(RawTypeList),
    TypeMap.

make_listmap() ->
    {Xsd,_Rest} = xmerl_scan:file("test.xsd", []),
    Raw = [{Name, Val} || #xmlElement{attributes=Attrs} <- xmerl_xpath:string("//xsd:element[@name][@type][@maxOccurs]", Xsd), 
                           #xmlAttribute{name=Name, value=Val} <- Attrs, Name =:= name orelse (Name =:= maxOccurs andalso Val =/= "1")].


parse_compliant(File, Schema) ->
    {ok, Model} = xmerl_xsd:process_schema("test.xsd"),
    %
    % GEN TYPE MAP
    %
    {Xsd,_Rest} = xmerl_scan:file("test.xsd", []),
    RawTypeList = lists:foldl(fun(Type, Acc) ->
       Acc ++ [ {list_to_binary(Name), Type} || {xmlAttribute,name,_,_,_,_Hierarchy,_,[],Name,_} <- xmerl_xpath:string("//xsd:element[@name][@type='" ++ Type ++ "']/@name", Xsd)] 
    end, [], ["xsd:integer", "xsd:date"]),
    TypeMap = maps:from_list(RawTypeList),
    % END GEN TYPE MAP

    %
    %   DRY RUN FOR VALIDATION
    %
    {StructForValidation,_Rest} = xmerl_scan:file("test.xml", []),
    %% END

    %
    % ACC FUN
    %%
    AccFun = fun(ParsedEntity, Acc, GlobalState) ->
        % ?PRINT(ParsedEntity),
        TransEntity = case ParsedEntity of
            #xmlText{parents=[{price, _} | _P], value=Val}=A -> 
                % ?PRINT(list_to_integer(Val)),
                Q = A#xmlText{value=list_to_integer(Val)},
                % ?PRINT(Q),
                Q;
            A -> A
        end,
        % ?PRINT(Acc),
        % ?PRINT(GlobalState),
        {[TransEntity|Acc], GlobalState}
    end,
    %% END
    case xmerl_xsd:validate(StructForValidation, Model) of
        {error, Error} -> {error, Error};
        _Other ->
            {Xml2,_Rest} = xmerl_scan:file("test.xml", [                
                    {space, normalize},
                    {acc_fun, AccFun}
                    % {fetch_fun, FetchFun}
                    % {hook_fun, HookFun},
                    % {event_fun, EventFun}
                    % {rules, ReadFun, WriteFun, ""}
            ]),
            % P = ,
            Q = xmerl_lib:remove_whitespace([Xml2]),
            Z = [simplify_element(T)|| T <- Q],
            ?PRINT(Z),
            Z
            
    end.


main () ->
    % parse_compliant("text.xml", "text.xsd").
    example_to_xml().



    % {ok, Model} = xmerl_xsd:process_schema("test.xsd"),
    % % xmerl_xsd:state2file(Model).

    

    % AccFun = fun(ParsedEntity, Acc, GlobalState) ->
    %     % ?PRINT(ParsedEntity),
    %     TransEntity = case ParsedEntity of
    %         #xmlText{parents=[{price, _} | _P], value=Val}=A -> 
    %             ?PRINT(list_to_integer(Val)),
    %             Q = A#xmlText{value=list_to_binary(Val)},
    %             ?PRINT(Q),
    %             Q;
    %         A -> A
    %     end,
    %     % ?PRINT(Acc),
    %     % ?PRINT(GlobalState),
    %     {[TransEntity|Acc], GlobalState}
    % end,

    % FetchFun = fun(URI, GlobalState) ->
    %     FetchState = xmerl_scan:fetch_state(GlobalState),
    %     ?PRINT(URI),
    %     {_, Filename} = URI,
    %     {ok, {file, Filename}, GlobalState}
    % end,

    % HookFun = fun(Entity, GlobalState) ->
    %     HookState = xmerl_scan:hook_state(GlobalState),
    %     TransEntity = case Entity of
    %         #xmlText{parents=[{price, _} | _P], value=Val}=A -> 
    %             ?PRINT(list_to_integer(Val)),
    %             Q = A#xmlText{value=list_to_integer(Val), type=cdata},
    %             ?PRINT(Q),
    %             Q;
    %         A -> A
    %     end,
    %     % ?PRINT(Entity),
    %     % {xmlElement,title,title,[],
    %         %  {xmlNamespace,'',[]},
    %         %  [{book,2},{book_store,1}],
    %         %  4,[],
    %         %  [{xmlText,[{title,4},{book,2},{book_store,1}],
    %         %            1,[],"Het zijn net mensen",text}],
    %         %  [],".",undeclared}
    %     % ?PRINT(HookState),
    %     % ?PRINT(GlobalState),
    %     GlobalState2 = xmerl_scan:hook_state(HookState, GlobalState),
    %     {Entity, GlobalState2}
    % end,
    
    % EventFun = fun(Entity, GlobalState) ->
    %     % ?PRINT(Entity),
    %     GlobalState
    % end,



    % WriteFun = fun(Context, Name, Definition, ScannerState) ->
    %     % ?PRINT(Context),
    %     ScannerState
    % end,

    % ReadFun = fun(Context, Name, ScannerState) ->
    %     % ?PRINT(Context),
    %     % ?PRINT(Name),
    %     % ?PRINT(ScannerState),
    %     undefined
    % end,
    
    % % {Xml1,_Rest} = xmerl_scan:file("test.xml", [
    % %     {validation, schema},
    % %     {schemaLocation, [{"","test.xsd"}]}
    % %     % {fetch_fun, FetchFun}

    % % ]),

    % {Xml2,_Rest} = xmerl_scan:file("test.xml", [
       
    %     {acc_fun, AccFun},
    %     % {fetch_fun, FetchFun}
    %     {hook_fun, HookFun},
    %     {event_fun, EventFun}

    %     % {rules, ReadFun, WriteFun, ""}

    % ]),
    % xmerl_xsd:validate(Xml2, Model).




















    % % erlsom:write_xsd_hrl_file("test.xsd", "test.hrl"),
    % % Struct = {book_store,[], [
    % %     {book_type,[],[
    % %         {title, [], "kak"}
        
    % %     ]}
    % %     ]},
    % {ok, Model} = erlsom:compile_xsd_file("test.xsd"),
    % % erlsom:write(Struct, Model).

    % {ok, Raw, _} = erlsom:scan_file("test.xml", Model),
    % Q = Raw#book_store{},
    % ?record_to_tuplelist(Q, book_store).

    % {ok, Model} = xmerl_xsd:process_schema("test.xsd"),

    % {ParsResult,Misc}=xmerl_scan:file("test.xml", [
    %     {validation,schema},
    %     {schemaLocation, ["test.xsd"]},
    %     {space, normalize}
    % ]),
    %  xmerl_xsd:validate(ParsResult, Model),
     
    % Struct = [{book_store,[], [
    %     {book,[],[
    %         {title, [], ["kak"]},
    %         {author, [], ["kak"]},
    %         {date, [], ["kak"]},            
    %         {publsher, [], ["kak"]}
        
    %         ]}
    %     ]}],
    % XML = ,
    % ?PRINT(XML),

    % {XML2,Misc2} = xmerl_scan:string(XML, []),
    % xmerl_xsd:validate(XML2, Model).


example_to_xml() ->
    Struct = [{book_store,[{xmlns, ""}], [
        {book,[],[
            {price, ["100000"]},
            {title, [], ["kak"]},
            {author, [], ["kak"]},
            {date, [], ["kak"]},            
            {publisher, [], ["kak"]}        
            ]}
        ]}],
    simple_form_to_compliant_xml(Struct, "test.xsd").




simple_form_to_compliant_xml([{Key, Attrs, Content}]=_Structure, XsdPath) ->
    {ok, Model} = xmerl_xsd:process_schema(XsdPath),
    Data = [{
        Key, 
        [] ++ Attrs,
        Content
    }],
   
    Xml = binary_to_list(list_to_binary(xmerl:export_simple(Data, xmerl_xml))),
   
    {Xml1,_Rest} = xmerl_scan:string(Xml, []),
    case xmerl_xsd:validate(Xml1, Model) of
        {error, Error} -> {error, Error};
        {_ValidElement, _Globalstate} -> {ok, Xml}
    end.
    

%
%   PRIV
%
%% We want simplification to yield a normal form, so we always generate
%% three-tuples for elements. PI, Comment and Decl elements are
%% discarded from content lists. Attribute values become flat
%% strings. Text elements are not flattened.

simplify_element(#xmlElement{expanded_name = [], name = Tag,
			     attributes = Attrs, content = Content}) ->
    {Tag, simplify_attributes(Attrs), simplify_content(Content)};
simplify_element(#xmlElement{expanded_name = Name,
			     attributes = Attrs, content = Content}) ->
    {Name, simplify_attributes(Attrs), simplify_content(Content)};
simplify_element(#xmlText{value = Text}) ->
    case is_integer(Text) of
        false -> list_to_binary(Text);
        true -> Text
    end;
simplify_element({Tag, Attrs, Content}) when is_atom(Tag) ->
    {Tag, simplify_attributes(Attrs), simplify_content(Content)};
simplify_element({Tag, Content}) when is_atom(Tag) ->
    {Tag, [], simplify_content(Content)};
simplify_element(Tag) when is_atom(Tag) ->
    {Tag, [], []};
simplify_element(Text) when is_list(Text) ->
    Text ++ "A".

simplify_content([#xmlPI{} | T]) ->
    simplify_content(T);
simplify_content([#xmlComment{} | T]) ->
    simplify_content(T);
simplify_content([#xmlDecl{} | T]) ->
    simplify_content(T);
simplify_content([H | T]) ->
    [simplify_element(H) | simplify_content(T)];
simplify_content([]) ->
    [].

simplify_attributes([#xmlAttribute{name = K, value = V} | T])
  when is_atom(K) ->
    [{K, expand_value(V)} | simplify_attributes(T)];
simplify_attributes([H = {K, _} | T]) when is_atom(K) ->
    [H | simplify_attributes(T)];
simplify_attributes([]) ->
    [].

expand_value(S) when is_atom(S) ->
    atom_to_list(S);
expand_value(S) when is_integer(S) ->
    integer_to_list(S) ++ "ss";
expand_value(S) ->
    flatten_text(S) ++ "sss".


%% Only flatten text.

flatten_text(T) ->
    flatten_text(T, []).

flatten_text([C | T], Cont) when is_integer(C) ->
    [C | flatten_text(T, Cont)];
flatten_text([T | T1], Cont) ->
    flatten_text(T, [T1 | Cont]);
flatten_text([], [T | Cont]) ->
    flatten_text(T, Cont);
flatten_text([], []) ->
    [];
flatten_text(Bin, Cont) ->
    flatten_text(binary_to_list(Bin), Cont).


remove_whitespace([#xmlText{value = " "} | Data]) ->
    remove_whitespace(Data);
remove_whitespace([E = #xmlElement{content = Content} | Data]) ->
    [E#xmlElement{content = remove_whitespace(Content)}
     | remove_whitespace(Data)];
remove_whitespace([Other | Data]) ->
    [Other | remove_whitespace(Data)];
remove_whitespace([]) ->
    [].