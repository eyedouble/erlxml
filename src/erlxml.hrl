
-record(erlXmlSchemaState, {id= make_ref(), struct, xsd_state}).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).