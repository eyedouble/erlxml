%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%% Description  : Utility module for handling XML trees.
%%%----------------------------------------------------------------------

-module(erlxml_lib).

-export([normalize_content/1, normalize_content/3, expand_content/1,
	 expand_content/3, normalize_element/1, normalize_element/3,
	 expand_element/1, expand_element/3, expand_attributes/1,
	 expand_attributes/3, export_text/1, flatten_text/1,
	 export_attribute/1, markup/2, markup/3, simplify_element/1,
	 simplify_content/1, start_tag/1, start_tag/2, end_tag/1,
	 empty_tag/1, empty_tag/2,is_empty_data/1, find_attribute/2,
	 remove_whitespace/1]).


-export([mapxml/2, foldxml/3, mapfoldxml/3]).


-include_lib("xmerl/include/xmerl.hrl").
-include("erlxml.hrl").

%% Escape special characters `<' and `&', flattening the text.
%% Also escapes `>', just for symmetry.

export_text(T) ->
    export_text(T, []).

export_text([$< | T], Cont) ->
    "&lt;" ++ export_text(T, Cont);
export_text([$> | T], Cont) ->
    "&gt;" ++ export_text(T, Cont);
export_text([$& | T], Cont) ->
    "&amp;" ++ export_text(T, Cont);
export_text([C | T], Cont) when is_integer(C) ->
    [C | export_text(T, Cont)];
export_text([T | T1], Cont) ->
    export_text(T, [T1 | Cont]);
export_text([], [T | Cont]) ->
    export_text(T, Cont);
export_text([], []) ->
    [];
export_text(Bin, Cont) ->
    export_text(binary_to_list(Bin), Cont).


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

%% Convert attribute value to a flat string, escaping characters `"',
%% `<' and `&'. (Note that single-quote characters are not escaped; the
%% markup-generating functions (`start_tag', `end_tag', ...) always use
%% `"' to delimit the attribute values.)

export_attribute(I) when is_integer(I) ->
    integer_to_list(I);
export_attribute(A) when is_atom(A) ->
    export_attribute(atom_to_list(A), []);
export_attribute(S) ->
    export_attribute(S, []).

export_attribute([$< | T], Cont) ->
    "&lt;" ++ export_attribute(T, Cont);
export_attribute([$& | T], Cont) ->
    "&amp;" ++ export_attribute(T, Cont);
export_attribute([$" | T], Cont) ->
    "&quot;" ++ export_attribute(T, Cont);
export_attribute([C | T], Cont) when is_integer(C) ->
    [C | export_attribute(T, Cont)];
export_attribute([T | T1], Cont) ->
    export_attribute(T, [T1 | Cont]);
export_attribute([], [T | Cont]) ->
    export_attribute(T, Cont);
export_attribute([], []) ->
    [];
export_attribute(Bin, Cont) ->
    export_attribute(binary_to_list(Bin), Cont).


%% SimpleContent: [SimpleElement]
%% SimpleElement: #xml...{} | String | {atom(), [Attr], SimpleContent}
%%                | {atom(), SimpleContent} | atom()
%% Attr: {atom(), Value} | #xmlAttribute{}
%% Value: atom() | integer() | String
%% String: [char() | binary() | String]
%%
%% Because strings can be deep, we do not allow content lists to also be
%% deep; otherwise, traversal of the simple representation becomes too
%% complicated and expensive. Simple content lists are thus flat lists
%% of simple elements.

%% TODO: namespace-qualified tags in simple-form? /RC

%% 'normalize' is like 'expand', but also turns all text elements into
%% flat strings.

normalize_element(Element) ->
    normalize_element(Element, 1, []).

normalize_element(Element, Pos, Parents) ->
    expand_element(Element, Pos, Parents, true).

%% 'expand' expands simple-form elements to normal XML elements.
%% All attribute values (also in #xmlAttribute records) become flat
%% strings, so that string comparisons can be made. Text elements are
%% not flattened.

expand_element(Element) ->
    expand_element(Element, 1, []).

expand_element(Element, Pos, Parents) ->
    expand_element(Element, Pos, Parents, false).

expand_element(E = #xmlElement{name = N}, Pos, Parents, Norm) ->
    NewParents = [{N,Pos}|Parents],
    Content = expand_content(E#xmlElement.content, 1, NewParents, Norm),
    Attrs = expand_attributes(E#xmlElement.attributes, 1, NewParents),
    E#xmlElement{pos = Pos,
		 parents = Parents,
		 attributes = Attrs,
		 content = Content};
expand_element(E = #xmlText{}, Pos, Parents, Norm) ->
    E#xmlText{pos = Pos,
	      parents = Parents,
	      value = expand_text(E#xmlText.value, Norm)};
expand_element(E = #xmlPI{}, Pos, _Parents, Norm) ->
    E#xmlPI{pos = Pos,
	    value = expand_text(E#xmlPI.value, Norm)};
expand_element(E = #xmlComment{}, Pos, Parents, Norm) ->
    E#xmlComment{pos = Pos,
		 parents = Parents,
		 value = expand_text(E#xmlComment.value, Norm)};
expand_element(E = #xmlDecl{}, _Pos, _Parents, _Norm) ->
    Attrs = expand_attributes(E#xmlDecl.attributes, 1, []),
    E#xmlDecl{attributes = Attrs};
expand_element({Tag, Attrs, Content}, Pos, Parents, Norm) when is_atom(Tag) ->

    % Namespace = case length([X||{AttrName, AttrVal}=X <- Attrs, (string:find(atom_to_list(AttrName), "xmlns") =/= nomatch)]) > 0 of
    %     true -> #xmlNamespace{default=[], nodes=[{"soap",'http://www.w3.org/2003/05/soap-envelope'},
    %         {"xsi",'http://www.w3.org/2001/XMLSchema-instance'},
    %         {"xsd",'http://www.w3.org/2001/XMLSchema'}]};
    %     false -> #xmlNamespace{}

    % end,

    NewParents = [{Tag, Pos} | Parents],
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = expand_attributes(Attrs, 1, NewParents),
		content = expand_content(Content, 1, NewParents, Norm)
    };
expand_element({Tag, Content}, Pos, Parents, Norm) when is_atom(Tag) ->
    NewParents = [{Tag, Pos} | Parents],
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = [],
		content = expand_content(Content, 1, NewParents, Norm)};
expand_element(Tag, Pos, Parents, _Norm) when is_atom(Tag) ->
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = [],
		content = []};
expand_element(String, Pos, Parents, Norm) when is_list(String) ->
    #xmlText{pos = Pos,
	     parents = Parents,
	     value = expand_text(String, Norm)}.

expand_text(S, false) -> S;
expand_text(S, true) -> flatten_text(S).

%% Content must be a flat list of elements.

normalize_content(Content) ->
    normalize_content(Content, 1, []).

normalize_content(Content, Pos, Parents) ->
    expand_content(Content, Pos, Parents, true).

expand_content(Content) ->
    expand_content(Content, 1, []).

expand_content(Content, Pos, Parents) ->
    expand_content(Content, Pos, Parents, false).

expand_content([{H} | T], Pos, Parents, Norm) ->
    expand_content(H ++ T, Pos, Parents, Norm);
expand_content([{F,S}|T], Pos, Parents, Norm) when is_function(F) ->
    case F(S) of
	done -> expand_content(T, Pos, Parents, Norm);
	{C,S2} -> expand_content([{F,S2},C|T], Pos, Parents, Norm)
    end;
expand_content([H | T], Pos, Parents, Norm) ->
    [expand_element(H, Pos, Parents, Norm)
     | expand_content(T, Pos+1, Parents, Norm)];
expand_content([], _Pos, _Parents, _Norm) ->
    [].

expand_attributes(Attrs) ->
    expand_attributes(Attrs, 1, []).

%% Expanding always turns all attribute values into flat strings.

expand_attributes([H = #xmlAttribute{} | T], Pos, Parents) ->
    [H#xmlAttribute{pos = Pos,
		    value = expand_value(H#xmlAttribute.value)}
     | expand_attributes(T, Pos+1, Parents)];
expand_attributes([{P,S}|T], Pos, Parents) when is_function(P) -> 
    case P(S) of
	done ->
	    expand_attributes(T, Pos, Parents);
	{A,S2} ->
	    expand_attributes([{P,S2},A|T], Pos, Parents)
    end;
expand_attributes([{K, V} | T], Pos, Parents) ->
    [#xmlAttribute{name = K,
		   pos = Pos,
		   parents = Parents,
		   value = expand_value(V)}
     | expand_attributes(T, Pos+1, Parents)];
expand_attributes([], _Pos, _Parents) ->
    [].

expand_value(S) when is_atom(S) ->
    atom_to_list(S);
expand_value(S) when is_integer(S) ->
    integer_to_list(S);
expand_value(S) ->
    flatten_text(S).

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
    Text;
simplify_element({Tag, Attrs, Content}) when is_atom(Tag) ->
    {Tag, simplify_attributes(Attrs), simplify_content(Content)};
simplify_element({Tag, Content}) when is_atom(Tag) ->
    {Tag, [], simplify_content(Content)};
simplify_element(Tag) when is_atom(Tag) ->
    {Tag, [], []};
simplify_element(Text) when is_list(Text) ->
    Text.

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

%% Looking up an attribute value

find_attribute(Name, Attrs) ->
    case lists:keysearch(Name, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = V}} ->
	    {value, V};
	false ->
	    false
    end.


markup(Tag, Data) ->
    markup(Tag, [], Data).

markup(Tag, Attrs, []) ->
    empty_tag(Tag, Attrs);
markup(Tag, Attrs, Data) ->
    [start_tag(Tag, Attrs), Data, end_tag(Tag)].

start_tag(TagStr) ->
    start_tag(TagStr, []).

start_tag(Tag, Attrs) when is_atom(Tag) ->
    start_tag(atom_to_list(Tag), Attrs);
start_tag(TagStr, []) ->
    ["<", TagStr, ">"];
start_tag(TagStr, Attrs) ->
    ["<", TagStr, attributes(Attrs), ">"].

empty_tag(Tag) ->
    empty_tag(Tag, []).

empty_tag(Tag, Attrs) when is_atom(Tag) ->
    empty_tag(atom_to_list(Tag), Attrs);
empty_tag(TagStr, []) ->
    ["<", TagStr, "/>"];
empty_tag(TagStr, Attrs) ->
    ["<", TagStr, attributes(Attrs), "/>"].

end_tag(Tag) when is_atom(Tag) ->
    end_tag(atom_to_list(Tag));
end_tag(TagStr) ->
    ["</", TagStr, ">"].

attributes(Attrs) ->
    [attr_string(A) || A <- Attrs].

attr_string(#xmlAttribute{name = K, value = V}) ->
    [" ", atom_to_list(K), "=\"", export_attribute(V), "\""].

is_empty_data([]) ->
    true;
is_empty_data([X | Xs]) ->
    case is_empty_data(X) of
	false ->
	    false;
	true ->
	    is_empty_data(Xs)
    end;
is_empty_data(_) ->
    false.


%% Removing normalised whitespace-only text segments.

remove_whitespace([#xmlText{value = " "} | Data]) ->
    remove_whitespace(Data);
remove_whitespace([E = #xmlElement{content = Content} | Data]) ->
    [E#xmlElement{content = remove_whitespace(Content)}
     | remove_whitespace(Data)];
remove_whitespace([Other | Data]) ->
    [Other | remove_whitespace(Data)];
remove_whitespace([]) ->
    [].


%%% ----------------------------------------------------------------------------
%%% funs traversing the xmerl tree left-right and top-down

%% mapxml
%% Fun is fun(Old#xmlElement) -> New#xmlElement
mapxml(Fun, #xmlElement{}= E) ->
    C1 = Fun(E),
    C2 = mapxml(Fun,lists:flatten(C1#xmlElement.content)),
    C1#xmlElement{content=C2};
mapxml(Fun, List) when is_list(List) ->
    AFun = fun(E) -> mapxml(Fun, E) end,
    lists:map(AFun, List);
mapxml(Fun, E) ->
    Fun(E).


%% foldxml
%% Fun is fun(#xmlElement, OldAccu) -> NewAccu
foldxml(Fun, Accu0, #xmlElement{content=C}=E) ->
    Accu1 = Fun(E, Accu0),
    foldxml(Fun, Accu1, C);
foldxml(Fun, Accu, List) when is_list(List) ->
    AFun = fun(E,A) -> foldxml(Fun, A, E) end,
    lists:foldl(AFun, Accu, List);
foldxml(Fun, Accu, E) ->
    Fun(E, Accu).


%% mapfoldxml
%% Fun is fun(Old#xmlElement, OldAccu) -> {New#xmlElement, NewAccu}
mapfoldxml(Fun, Accu0, #xmlElement{}=E) ->
    {C1,Accu1} = Fun(E, Accu0),
    {C2,Accu2} = mapfoldxml(Fun, Accu1, lists:flatten(C1#xmlElement.content)),
    {C1#xmlElement{content=C2},Accu2};
mapfoldxml(Fun, Accu, List) when is_list(List) ->
    AFun = fun(E,A) -> mapfoldxml(Fun, A, E) end,
    lists:mapfoldl(AFun, Accu, List);
mapfoldxml(Fun, Accu, E) ->
    Fun(E,Accu).




