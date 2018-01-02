-module(lib_misc).
-export([dump/2]).

dump(File, Term) ->
	Out = File ++ ".tmp",
	io:format("** dumping ot ~s~n", [Out]),
	{ok, S} = file:open(Out, [write]),
	io:format(S, "~p.~n", [Term]),
	file:close(S).

