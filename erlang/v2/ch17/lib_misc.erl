-module(lib_misc).
-export([string2value/1, sleep/1]).

string2value(Str) ->
	{ok, Tokens, _} = erl_scan:string(Str ++ "."),
	{ok, Exprs} = erl_parse:parse_exprs(Tokens),
	Bindings = erl_eval:new_bindings(),
	{value, Value, _} = erl_eval:exprs(Exprs, Bindings),
	Value.

sleep(T) ->
	receive
	after T ->
		true
	end.

